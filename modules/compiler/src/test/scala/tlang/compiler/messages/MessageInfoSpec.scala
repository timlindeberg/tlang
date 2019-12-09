package tlang
package compiler
package messages

import tlang.formatting.Colors
import tlang.formatting.textformatters.{SyntaxHighlighter, TabReplacer}
import tlang.testutils.UnitSpec
import tlang.utils._

class MessageInfoSpec extends UnitSpec with MessageTesting {

  val DefaultContextSize = 2
  val DefaultTabWidth = 2

  var messageInfo: MessageInfo = _

  behavior of "A message formatter"

  it should "use correct color depending on the messagetype" in {
    import Colors._

    messageInfo = getMessageInfo(createMessage(messageType = MessageType.Warning), useColor = true)
    messageInfo.color shouldBe (Bold + Yellow)

    messageInfo = getMessageInfo(createMessage(messageType = MessageType.Error), useColor = true)
    messageInfo.color shouldBe (Bold + Red)

    messageInfo = getMessageInfo(createMessage(messageType = MessageType.Fatal), useColor = true)
    messageInfo.color shouldBe (Bold + Red)

    messageInfo = getMessageInfo(createMessage(messageType = MessageType.Fatal), useColor = false)
    messageInfo.color shouldBe NoColor
  }

  it should "use the correct prefix" in {
    messageInfo = getMessageInfo(createMessage(messageType = MessageType.Warning, errorLetters = "ABC", codeNum = 123), useColor = true)

    messageInfo.prefix should matchWithAnsi("\u001b[1;33mWarning ABC1123\u001b[0m")

    messageInfo = getMessageInfo(createMessage(messageType = MessageType.Error, errorLetters = "DEF", codeNum = 1), useColor = true)
    messageInfo.prefix should matchWithAnsi("\u001b[1;31mError DEF2001\u001b[0m")

    messageInfo = getMessageInfo(createMessage(messageType = MessageType.Fatal, errorLetters = "GHI", codeNum = 23), useColor = true)
    messageInfo.prefix should matchWithAnsi("\u001b[1;31mFatal GHI3023\u001b[0m")

    messageInfo = getMessageInfo(createMessage(messageType = MessageType.Warning, errorLetters = "A", codeNum = 5), useColor = false)
    messageInfo.prefix should matchWithAnsi("Warning A1005")
  }

  it should "show position correctly" in {
    val message = createMessage(pos = Position(1, 10, 100, 1000))
    messageInfo = getMessageInfo(message, useColor = true)
    messageInfo.positionDescription should matchWithAnsi("\u001b[1;35m1\u001b[0m:\u001b[1;35m10\u001b[0m")

    messageInfo = getMessageInfo(message, useColor = false)
    messageInfo.positionDescription should matchWithAnsi("1:10")
  }

  it should "show source description correctly" in {
    val source = mock[Source]
    source.lines returns IndexedSeq()

    val posWithFile = Position(1, 10, 100, 1000, source = Some(source))
    val message = createMessage(pos = posWithFile)
    messageInfo = getMessageInfo(message, useColor = true)
    source.description(*) returns s"core/src/test/resources/positions/\u001b[1;35mParserPositions.t\u001b[0m"

    messageInfo.sourceDescription should matchWithAnsi(
      s"core/src/test/resources/positions/\u001b[1;35mParserPositions.t\u001b[0m:\u001b[1;35m1\u001b[0m:\u001b[1;35m10\u001b[0m"
    )

    // Without color
    messageInfo = getMessageInfo(message, useColor = false)
    source.description(*) returns s"core/src/test/resources/positions/ParserPositions.t"

    messageInfo.sourceDescription should matchWithAnsi(s"core/src/test/resources/positions/ParserPositions.t:1:10")
  }

  it should "show the location with color" in {
    val source = mock[Source]
    source.lines returns IndexedSeq("for(var i = x; i < 5; i++)")

    // the error is in "i < 5"
    val errorPos = Position(1, 16, 1, 21, source = Some(source))

    val message = createMessage(pos = errorPos)
    messageInfo = getMessageInfo(message, useColor = true, contextSize = 0)

    val lines = messageInfo.locationInSource
    lines should have size 1

    val (num, line) = lines(0)
    num should matchWithAnsi("\u001b[35m1\u001b[0m")
    line should matchWithAnsi("for(var i = x; \u001b[1;4;31mi < 5\u001b[0m; i++)")
  }

  it should "show the location in the file without colors" in {
    val source = mock[Source]
    source.lines returns IndexedSeq("for(var i = x; i < 5; i++)")
    // the error is in "i < 5"
    val errorPos = Position(1, 16, 1, 21, source = Some(source))
    val message = createMessage(messageType = MessageType.Warning, pos = errorPos)

    messageInfo = getMessageInfo(message, contextSize = 1, useColor = false)

    val lines = messageInfo.locationInSource
    lines should have size 2

    val (num1, line1) = lines(0)
    num1 should matchWithAnsi("1")
    line1 should matchWithAnsi("for(var i = x; i < 5; i++)")

    val (num2, line2) = lines(1)
    num2 should matchWithAnsi("")
    line2 should matchWithAnsi("               ~~~~~")
  }

  it should "handle different context sizes" in {
    val source = mock[Source]
    source.lines returns IndexedSeq(
      "var a = 0",
      "var b = 0",
      "var c = 0",
      "var d = 0",
      "var e = 0",
      "for(var i = x; i < 5; i++)",
      "\t\ta++",
      "\t\tb++",
      "\t\tc++",
      "\t\td++",
      "\t\te++"
    )
    // the error is in "i < 5"
    val message = createMessage(pos = Position(6, 16, 6, 21, source = Some(source)))

    test("No context lines") {
      test("Without color") {
        messageInfo = getMessageInfo(message, contextSize = 0, useColor = false)
        val lines = messageInfo.locationInSource
        lines shouldBe List(
          ("6", "for(var i = x; i < 5; i++)"),
          ("", "               ~~~~~")
        )
      }

      test("With color") {
        val messageInfoColored = getMessageInfo(message, contextSize = 0, useColor = true)
        val (nums, lines) = messageInfoColored.locationInSource.unzip
        nums should allMatchWithAnsi(
          "\u001b[35m6\u001b[0m"
        )
        lines should allMatchWithAnsi(
          "for(var i = x; \u001b[1;4;31mi < 5\u001b[0m; i++)"
        )
      }
    }

    test("Context size 1") {
      test("Without color") {
        val messageInfo = getMessageInfo(message, contextSize = 1, useColor = false)
        val lines = messageInfo.locationInSource
        lines shouldBe List(
          ("5", "var e = 0"),
          ("6", "for(var i = x; i < 5; i++)"),
          ("", "               ~~~~~"),
          ("7", "    a++")
        )
      }

      test("With color") {
        val messageInfo = getMessageInfo(message, contextSize = 1, useColor = true)
        val (nums, lines) = messageInfo.locationInSource.unzip
        nums should allMatchWithAnsi(
          "\u001b[35m5\u001b[0m",
          "\u001b[35m6\u001b[0m",
          "\u001b[35m7\u001b[0m"
        )
        lines should allMatchWithAnsi(
          "var e = 0",
          "for(var i = x; \u001b[1;4;31mi < 5\u001b[0m; i++)",
          "    a++"
        )
      }
    }

    test("Context size 2") {
      test("With color") {
        val messageInfo = getMessageInfo(message, contextSize = 2, useColor = false)
        val lines = messageInfo.locationInSource
        lines shouldBe List(
          ("4", "var d = 0"),
          ("5", "var e = 0"),
          ("6", "for(var i = x; i < 5; i++)"),
          ("", "               ~~~~~"),
          ("7", "    a++"),
          ("8", "    b++")
        )
      }

      test("With color") {
        val messageInfo = getMessageInfo(message, contextSize = 2, useColor = true)
        val (nums, lines) = messageInfo.locationInSource.unzip
        nums should allMatchWithAnsi(
          "\u001b[35m4\u001b[0m",
          "\u001b[35m5\u001b[0m",
          "\u001b[35m6\u001b[0m",
          "\u001b[35m7\u001b[0m",
          "\u001b[35m8\u001b[0m"
        )
        lines should allMatchWithAnsi(
          "var d = 0",
          "var e = 0",
          "for(var i = x; \u001b[1;4;31mi < 5\u001b[0m; i++)",
          "    a++",
          "    b++"
        )
      }
    }

    test("Context size 5") {
      test("Without color") {
        val messageInfo = getMessageInfo(message, contextSize = 5, useColor = false)
        val lines = messageInfo.locationInSource
        lines shouldBe List(
          ("1", "var a = 0"),
          ("2", "var b = 0"),
          ("3", "var c = 0"),
          ("4", "var d = 0"),
          ("5", "var e = 0"),
          ("6", "for(var i = x; i < 5; i++)"),
          ("", "               ~~~~~"),
          ("7", "    a++"),
          ("8", "    b++"),
          ("9", "    c++"),
          ("10", "    d++"),
          ("11", "    e++")
        )
      }

      test("With color") {
        val messageInfo = getMessageInfo(message, contextSize = 5, useColor = true)
        val (nums, lines) = messageInfo.locationInSource.unzip
        nums should allMatchWithAnsi(
          "\u001b[35m1\u001b[0m",
          "\u001b[35m2\u001b[0m",
          "\u001b[35m3\u001b[0m",
          "\u001b[35m4\u001b[0m",
          "\u001b[35m5\u001b[0m",
          "\u001b[35m6\u001b[0m",
          "\u001b[35m7\u001b[0m",
          "\u001b[35m8\u001b[0m",
          "\u001b[35m9\u001b[0m",
          "\u001b[35m10\u001b[0m",
          "\u001b[35m11\u001b[0m"
        )
        lines shouldBe List(
          "var a = 0",
          "var b = 0",
          "var c = 0",
          "var d = 0",
          "var e = 0",
          "for(var i = x; \u001b[1;4;31mi < 5\u001b[0m; i++)",
          "    a++",
          "    b++",
          "    c++",
          "    d++",
          "    e++"
        )
      }
    }

    test("Context larger than number of lines") {
      test("Without color") {
        val messageInfo = getMessageInfo(message, contextSize = 5, useColor = false)
        val lines = messageInfo.locationInSource
        lines shouldBe List(
          ("1", "var a = 0"),
          ("2", "var b = 0"),
          ("3", "var c = 0"),
          ("4", "var d = 0"),
          ("5", "var e = 0"),
          ("6", "for(var i = x; i < 5; i++)"),
          ("", "               ~~~~~"),
          ("7", "    a++"),
          ("8", "    b++"),
          ("9", "    c++"),
          ("10", "    d++"),
          ("11", "    e++")
        )
      }

      test("With color") {
        val messageInfo = getMessageInfo(message, contextSize = 5, useColor = true)
        val (nums, lines) = messageInfo.locationInSource.unzip
        nums should allMatchWithAnsi(
          "\u001b[35m1\u001b[0m",
          "\u001b[35m2\u001b[0m",
          "\u001b[35m3\u001b[0m",
          "\u001b[35m4\u001b[0m",
          "\u001b[35m5\u001b[0m",
          "\u001b[35m6\u001b[0m",
          "\u001b[35m7\u001b[0m",
          "\u001b[35m8\u001b[0m",
          "\u001b[35m9\u001b[0m",
          "\u001b[35m10\u001b[0m",
          "\u001b[35m11\u001b[0m"
        )
        lines shouldBe List(
          "var a = 0",
          "var b = 0",
          "var c = 0",
          "var d = 0",
          "var e = 0",
          "for(var i = x; \u001b[1;4;31mi < 5\u001b[0m; i++)",
          "    a++",
          "    b++",
          "    c++",
          "    d++",
          "    e++"
        )
      }
    }

    test("Error at first line, should only have context lines below") {
      val message = createMessage(pos = Position(1, 5, 1, 6, source = Some(source)))

      test("Without color") {
        val messageInfo = getMessageInfo(message, contextSize = 2, useColor = false)
        val lines = messageInfo.locationInSource
        lines should have size 4
        lines shouldBe List(
          ("1", "var a = 0"),
          ("", "    ~"),
          ("2", "var b = 0"),
          ("3", "var c = 0")
        )
      }

      test("With color") {
        val messageInfo = getMessageInfo(message, contextSize = 2, useColor = true)
        val (nums, lines) = messageInfo.locationInSource.unzip

        nums should allMatchWithAnsi(
          "\u001b[35m1\u001b[0m",
          "\u001b[35m2\u001b[0m",
          "\u001b[35m3\u001b[0m"
        )
        lines shouldBe List(
          "var \u001b[1;4;31ma\u001b[0m = 0",
          "var b = 0",
          "var c = 0"
        )
      }
    }

    test("Error at second line, should only have one context line above") {
      val message = createMessage(pos = Position(2, 5, 2, 6, source = Some(source)))

      test("Without color") {
        val messageInfo = getMessageInfo(message, contextSize = 2, useColor = false)
        val lines = messageInfo.locationInSource
        lines should have size 5
        lines shouldBe List(
          ("1", "var a = 0"),
          ("2", "var b = 0"),
          ("", "    ~"),
          ("3", "var c = 0"),
          ("4", "var d = 0")
        )
      }

      test("With color") {
        val messageInfo = getMessageInfo(message, contextSize = 2, useColor = true)
        val (nums, lines) = messageInfo.locationInSource.unzip

        nums should allMatchWithAnsi(
          "\u001b[35m1\u001b[0m",
          "\u001b[35m2\u001b[0m",
          "\u001b[35m3\u001b[0m",
          "\u001b[35m4\u001b[0m"
        )
        lines shouldBe List(
          "var a = 0",
          "var \u001b[1;4;31mb\u001b[0m = 0",
          "var c = 0",
          "var d = 0"
        )
      }
    }

    test("Error at last line, should only have context lines above") {
      // Indentation should also be trimmed for these lines

      val message = createMessage(pos = Position(11, 3, 11, 4, source = Some(source)))

      test("Without color") {
        val messageInfo = getMessageInfo(message, contextSize = 2, useColor = false)
        val lines = messageInfo.locationInSource
        lines should have size 4
        lines shouldBe List(
          ("9", "c++"),
          ("10", "d++"),
          ("11", "e++"),
          ("", "~")
        )
      }

      test("With color") {
        val messageInfo = getMessageInfo(message, contextSize = 2, useColor = true)
        val (nums, lines) = messageInfo.locationInSource.unzip

        nums should allMatchWithAnsi(
          "\u001b[35m9\u001b[0m",
          "\u001b[35m10\u001b[0m",
          "\u001b[35m11\u001b[0m"
        )
        lines shouldBe List(
          "c++",
          "d++",
          "\u001b[1;4;31me\u001b[0m++"
        )
      }
    }
  }

  it should "trim indentation of location" in {
    val source = mock[Source]
    source.lines returns IndexedSeq(
      "\t\t\tvar a = 0",
      "\t\t\tfor(var i = x; i < 5; i++)",
      "\t\t\t\ta++"
    )

    // the error is in "i < 5"
    val errorPos = Position(2, 19, 2, 24, source = Some(source))
    val message = createMessage(messageType = MessageType.Warning, pos = errorPos)

    test("Without color") {
      val messageInfo = getMessageInfo(message, useColor = false)
      val lines = messageInfo.locationInSource
      lines should have size 4
      lines shouldBe List(
        ("1", "var a = 0"),
        ("2", "for(var i = x; i < 5; i++)"),
        ("", "               ~~~~~"),
        ("3", "  a++")
      )
    }

    test("With color") {
      val messageInfo = getMessageInfo(message, useColor = true, syntaxHighlighter = mockedSyntaxHighlighter)
      val (nums, lines) = messageInfo.locationInSource.unzip
      nums should allMatchWithAnsi(
        "\u001b[35m1\u001b[0m",
        "\u001b[35m2\u001b[0m",
        "\u001b[35m3\u001b[0m"
      )
      lines should allMatchWithAnsi(
        "var a = 0",
        "for(var i = x; \u001b[1;4;33mi < 5\u001b[0m; i++)",
        "  a++"
      )
    }
  }

  it should "replace tabs in location" in {
    val source = mock[Source]
    source.lines returns IndexedSeq(
      "\t\tvar a = 0",
      "for(var i = x; i < 5; i++)",
      "\t\ta++"
    )

    // the error is in "i < 5"
    val message = createMessage(messageType = MessageType.Warning, pos = Position(2, 16, 2, 21, source = Some(source)))

    test("Normal tab width") {
      test("Without color") {
        val messageInfo = getMessageInfo(message, useColor = false, tabWidth = 2)
        val lines = messageInfo.locationInSource
        lines should have size 4
        lines shouldBe List(
          ("1", "    var a = 0"),
          ("2", "for(var i = x; i < 5; i++)"),
          ("", "               ~~~~~"),
          ("3", "    a++")
        )
      }

      test("With color") {
        val messageInfo = getMessageInfo(message, useColor = true, tabWidth = 2)
        val (nums, lines) = messageInfo.locationInSource.unzip
        nums should allMatchWithAnsi(
          "\u001b[35m1\u001b[0m",
          "\u001b[35m2\u001b[0m",
          "\u001b[35m3\u001b[0m"
        )
        lines should allMatchWithAnsi(
          "    var a = 0",
          "for(var i = x; \u001b[1;4;33mi < 5\u001b[0m; i++)",
          "    a++"
        )
      }
    }

    test("Tab width 6") {
      test("Without color") {
        val messageInfo = getMessageInfo(message, useColor = false, tabWidth = 6)
        val lines = messageInfo.locationInSource
        lines should have size 4
        lines shouldBe List(
          ("1", "            var a = 0"),
          ("2", "for(var i = x; i < 5; i++)"),
          ("", "               ~~~~~"),
          ("3", "            a++")
        )
      }

      test("With color") {
        val messageInfo = getMessageInfo(message, useColor = true, tabWidth = 6)
        val (nums, lines) = messageInfo.locationInSource.unzip
        nums should allMatchWithAnsi(
          "\u001b[35m1\u001b[0m",
          "\u001b[35m2\u001b[0m",
          "\u001b[35m3\u001b[0m"
        )
        lines should allMatchWithAnsi(
          "            var a = 0",
          "for(var i = x; \u001b[1;4;33mi < 5\u001b[0m; i++)",
          "            a++"
        )
      }
    }

    test("Error where tabs get replaced") {
      // error in a++
      val message = createMessage(messageType = MessageType.Warning, pos = Position(3, 3, 3, 6, source = Some(source)))

      test("Without color") {
        val messageInfo = getMessageInfo(message, useColor = false, tabWidth = 6)
        val lines = messageInfo.locationInSource
        lines should have size 4
        lines shouldBe List(
          ("1", "            var a = 0"),
          ("2", "for(var i = x; i < 5; i++)"),
          ("3", "            a++"),
          ("", "            ~~~")
        )
      }

      test("With color") {
        val messageInfo = getMessageInfo(message, useColor = true, tabWidth = 6)
        val (nums, lines) = messageInfo.locationInSource.unzip
        nums should allMatchWithAnsi(
          "\u001b[35m1\u001b[0m",
          "\u001b[35m2\u001b[0m",
          "\u001b[35m3\u001b[0m"
        )
        lines should allMatchWithAnsi(
          "            var a = 0",
          "for(var i = x; i < 5; i++)",
          "            \u001b[1;4;33ma++\u001b[0m"
        )
      }
    }
  }

  it should "mark errors over multiple lines" in {
    var source = mock[Source]
    source.lines returns IndexedSeq(
      "for(var i = x; i < 5; i++)",
      "\t\ta++ // abcdef",
      "\t\tb++"
    )

    // the error is from "a" all the way to the end
    var errorPos = Position(2, 3, 3, 6, source = Some(source))
    var message: CompilerMessage = createMessage(messageType = MessageType.Warning, pos = errorPos)

    var messageInfo = getMessageInfo(message, useColor = false)
    var lines = messageInfo.locationInSource
    lines should have size 5
    lines shouldBe List(
      ("1", "for(var i = x; i < 5; i++)"),
      ("2", "    a++ // abcdef"), // error indicator should continue over the whole line
      ("", "    ~~~~~~~~~~~~~"),
      ("3", "    b++"),
      ("", "    ~~~")
    )

    source = mock[Source]
    source.lines returns IndexedSeq(
      "\t\telse",
      "\t\t\treturn // res: F1002",
      "\t\t\tprintln(a) // res: F1000"
    )
    errorPos = Position(2, 1, 3, 28, source = Some(source))
    message = createMessage(messageType = MessageType.Warning, pos = errorPos)

    messageInfo = getMessageInfo(message, useColor = false)
    lines = messageInfo.locationInSource
    lines should have size 5
    lines shouldBe List(
      ("1", "else"),
      ("2", "  return // res: F1002"), // error indicator should continue over the whole line
      ("", "  ~~~~~~~~~~~~~~~~~~~~"),
      ("3", "  println(a) // res: F1000"),
      ("", "  ~~~~~~~~~~~~~~~~~~~~~~~~")
    )

    // try last case with colors as well
    messageInfo = getMessageInfo(message, useColor = true)
    lines = messageInfo.locationInSource
    lines should have size 3
    val (nums, texts) = lines.unzip
    nums should allMatchWithAnsi(
      "\u001b[35m1\u001b[0m",
      "\u001b[35m2\u001b[0m",
      "\u001b[35m3\u001b[0m"
    )
    texts should allMatchWithAnsi(
      "else",
      "  \u001b[1;4;33mreturn // res: F1002\u001b[0m",
      "  \u001b[1;4;33mprintln(a) // res: F1000\u001b[0m"
    )
  }

  private def getMessageInfo(
    message: CompilerMessage,
    useColor: Boolean = true,
    contextSize: Int = DefaultContextSize,
    tabWidth: Int = DefaultTabWidth,
    syntaxHighlighter: SyntaxHighlighter = mockedSyntaxHighlighter
  ): MessageInfo = {
    val formatter = testFormatter(
      syntaxHighlighter = syntaxHighlighter,
      tabReplacer = TabReplacer(tabWidth),
      useColor = useColor
    )
    MessageInfo(message, syntaxHighlighter, contextSize)(formatter)
  }
}
