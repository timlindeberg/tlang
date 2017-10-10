package tlang.formatting

import tlang.formatting.textformatters.{SyntaxHighlighter, TabReplacer}
import tlang.messages._
import tlang.testutils.UnitSpec
import tlang.utils.Extensions._
import tlang.utils._

class MessageFormatterSpec extends UnitSpec {

  val DefaultContextSize = 2
  val DefaultTabWidth    = 2


  behavior of "A message formatter"


  it should "use correct color depending on the messagetype" in {
    var messageFormatter = getMessageFormatter(useColor = true)

    import Colors._

    messageFormatter setMessage createMessage(messageType = MessageType.Warning)
    messageFormatter.color shouldBe (Bold + Yellow)

    messageFormatter setMessage createMessage(messageType = MessageType.Error)
    messageFormatter.color shouldBe (Bold + Red)

    messageFormatter setMessage createMessage(messageType = MessageType.Fatal)
    messageFormatter.color shouldBe (Bold + Red)

    messageFormatter = getMessageFormatter(useColor = false)
    messageFormatter setMessage createMessage(messageType = MessageType.Fatal)
    messageFormatter.color shouldBe NoColor
  }


  it should "use the correct prefix" in {
    var messageFormatter = getMessageFormatter(useColor = true)
    messageFormatter setMessage createMessage(messageType = MessageType.Warning, errorLetters = "ABC", codeNum = 123)

    messageFormatter.prefix should matchWithAnsi("\u001b[1;33mWarning ABC1123\u001b[0m")

    messageFormatter setMessage createMessage(messageType = MessageType.Error, errorLetters = "DEF", codeNum = 1)
    messageFormatter.prefix should matchWithAnsi("\u001b[1;31mError DEF2001\u001b[0m")

    messageFormatter setMessage createMessage(messageType = MessageType.Fatal, errorLetters = "GHI", codeNum = 23)
    messageFormatter.prefix should matchWithAnsi("\u001b[1;31mFatal GHI3023\u001b[0m")

    messageFormatter = getMessageFormatter(useColor = false)
    messageFormatter setMessage createMessage(messageType = MessageType.Warning, errorLetters = "A", codeNum = 5)
    messageFormatter.prefix should matchWithAnsi("Warning A1005")

  }


  it should "show position correctly" in {
    var messageFormatter = getMessageFormatter(useColor = true)
    val message = createMessage(pos = PositionWithSource(1, 10, 100, 1000))
    messageFormatter setMessage message
    messageFormatter.positionDescription should matchWithAnsi("\u001b[1;35m1\u001b[0m:\u001b[1;35m10\u001b[0m")

    messageFormatter = getMessageFormatter(useColor = false)
    messageFormatter setMessage message
    messageFormatter.positionDescription should matchWithAnsi("1:10")
  }


  it should "show source description correctly" in {
    val formattingWithColor = Formatting(useColor = true)
    val formattingWithoutColor = Formatting(useColor = false)

    val source = mock[Source]
    source.lines returns IndexedSeq()
    source.description(formattingWithColor) returns s"core/src/test/resources/positions/\u001b[1;35mParserPositions.t\u001b[0m"

    source.description(formattingWithoutColor) returns s"core/src/test/resources/positions/ParserPositions.t"


    var messageFormatter = getMessageFormatter(useColor = true, formatting = Some(formattingWithColor))

    val posWithFile = PositionWithSource(1, 10, 100, 1000, source = Some(source))
    messageFormatter setMessage createMessage(pos = posWithFile)

    messageFormatter.sourceDescription should matchWithAnsi(
      s"\u001b[1;35m1\u001b[0m:\u001b[1;35m10\u001b[0m core/src/test/resources/positions/\u001b[1;35mParserPositions.t\u001b[0m"
    )

    // Without color
    messageFormatter = getMessageFormatter(useColor = false, formatting = Some(formattingWithoutColor))
    messageFormatter setMessage createMessage(pos = posWithFile)
    messageFormatter.sourceDescription should matchWithAnsi(s"1:10 core/src/test/resources/positions/ParserPositions.t")
  }


  it should "show the location with color" in {
    val source = mock[Source]
    source.lines returns IndexedSeq("for(var i = x; i < 5; i++)")

    // the error is in "i < 5"
    val errorPos = PositionWithSource(1, 16, 1, 21, source = Some(source))

    val messageFormatter = getMessageFormatter(useColor = true, contextSize = 0)

    messageFormatter setMessage createMessage(pos = errorPos)

    val lines = messageFormatter.locationInSource
    lines should have size 1

    val (num, line) = lines(0)
    num should matchWithAnsi("\u001b[35m1\u001b[0m")
    line should matchWithAnsi("for(var i = x; \u001b[1;4;31mi < 5\u001b[0m; i++)")
  }


  it should "show the location in the file without colors" in {
    val source = mock[Source]
    source.lines returns IndexedSeq("for(var i = x; i < 5; i++)")
    // the error is in "i < 5"
    val errorPos = PositionWithSource(1, 16, 1, 21, source = Some(source))

    val messageFormatter = getMessageFormatter(contextSize = 1, useColor = false)

    messageFormatter setMessage createMessage(messageType = MessageType.Warning, pos = errorPos)

    val lines = messageFormatter.locationInSource
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
    val message = createMessage(pos = PositionWithSource(6, 16, 6, 21, source = Some(source)))

    test("No context lines") {

      test("Without color") {
        val messageFormatter = getMessageFormatter(contextSize = 0, useColor = false, message = Some(message))
        val lines = messageFormatter.locationInSource
        lines shouldBe List(
          ("6", "for(var i = x; i < 5; i++)"),
          ("", "               ~~~~~")
        )
      }

      test("With color") {
        val messageFormatterColored = getMessageFormatter(contextSize = 0, useColor = true, message = Some(message))
        val (nums, lines) = messageFormatterColored.locationInSource.unzip
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
        val messageFormatter = getMessageFormatter(contextSize = 1, useColor = false, message = Some(message))
        val lines = messageFormatter.locationInSource
        lines shouldBe List(
          ("5", "var e = 0"),
          ("6", "for(var i = x; i < 5; i++)"),
          ("", "               ~~~~~"),
          ("7", "    a++")
        )
      }

      test("With color") {
        val messageFormatter = getMessageFormatter(contextSize = 1, useColor = true, message = Some(message))
        val (nums, lines) = messageFormatter.locationInSource.unzip
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
        val messageFormatter = getMessageFormatter(contextSize = 2, useColor = false, message = Some(message))
        val lines = messageFormatter.locationInSource
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
        val messageFormatter = getMessageFormatter(contextSize = 2, useColor = true, message = Some(message))
        val (nums, lines) = messageFormatter.locationInSource.unzip
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
        val messageFormatter = getMessageFormatter(contextSize = 5, useColor = false, message = Some(message))
        val lines = messageFormatter.locationInSource
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
        val messageFormatter = getMessageFormatter(contextSize = 5, useColor = true, message = Some(message))
        val (nums, lines) = messageFormatter.locationInSource.unzip
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
        val messageFormatter = getMessageFormatter(contextSize = 5, useColor = false, message = Some(message))
        val lines = messageFormatter.locationInSource
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
        val messageFormatter = getMessageFormatter(contextSize = 5, useColor = true, message = Some(message))
        val (nums, lines) = messageFormatter.locationInSource.unzip
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
      val message = createMessage(pos = PositionWithSource(1, 5, 1, 6, source = Some(source)))

      test("Without color") {
        val messageFormatter = getMessageFormatter(contextSize = 2, useColor = false, message = Some(message))
        val lines = messageFormatter.locationInSource
        lines should have size 4
        lines shouldBe List(
          ("1", "var a = 0"),
          ("", "    ~"),
          ("2", "var b = 0"),
          ("3", "var c = 0")
        )
      }

      test("With color") {
        val messageFormatter = getMessageFormatter(contextSize = 2, useColor = true, message = Some(message))
        val (nums, lines) = messageFormatter.locationInSource.unzip

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
      val message = createMessage(pos = PositionWithSource(2, 5, 2, 6, source = Some(source)))

      test("Without color") {
        val messageFormatter = getMessageFormatter(contextSize = 2, useColor = false, message = Some(message))
        val lines = messageFormatter.locationInSource
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
        val messageFormatter = getMessageFormatter(contextSize = 2, useColor = true, message = Some(message))
        val (nums, lines) = messageFormatter.locationInSource.unzip

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

      val message = createMessage(pos = PositionWithSource(11, 3, 11, 4, source = Some(source)))

      test("Without color") {
        val messageFormatter = getMessageFormatter(contextSize = 2, useColor = false, message = Some(message))
        val lines = messageFormatter.locationInSource
        lines should have size 4
        lines shouldBe List(
          ("9", "c++"),
          ("10", "d++"),
          ("11", "e++"),
          ("", "~")
        )
      }

      test("With color") {
        val messageFormatter = getMessageFormatter(contextSize = 2, useColor = true, message = Some(message))
        val (nums, lines) = messageFormatter.locationInSource.unzip

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
    val errorPos = PositionWithSource(2, 19, 2, 24, source = Some(source))
    val message = createMessage(messageType = MessageType.Warning, pos = errorPos)

    test("Without color") {
      val messageFormatter = getMessageFormatter(useColor = false, message = Some(message))
      val lines = messageFormatter.locationInSource
      lines should have size 4
      lines shouldBe List(
        ("1", "var a = 0"),
        ("2", "for(var i = x; i < 5; i++)"),
        ("", "               ~~~~~"),
        ("3", "  a++")
      )
    }

    test("With color") {
      val messageFormatter = getMessageFormatter(useColor = true, message = Some(message), syntaxHighlighter = mockedSyntaxHighlighter)
      val (nums, lines) = messageFormatter.locationInSource.unzip
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
    val message = createMessage(messageType = MessageType.Warning, pos = PositionWithSource(2, 16, 2, 21, source = Some(source)))

    test("Normal tab width") {

      test("Without color") {
        val messageFormatter = getMessageFormatter(useColor = false, tabWidth = 2, message = Some(message))
        val lines = messageFormatter.locationInSource
        lines should have size 4
        lines shouldBe List(
          ("1", "    var a = 0"),
          ("2", "for(var i = x; i < 5; i++)"),
          ("", "               ~~~~~"),
          ("3", "    a++")
        )
      }

      test("With color") {
        val messageFormatter = getMessageFormatter(useColor = true, tabWidth = 2, message = Some(message))
        val (nums, lines) = messageFormatter.locationInSource.unzip
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
        val messageFormatter = getMessageFormatter(useColor = false, tabWidth = 6, message = Some(message))
        val lines = messageFormatter.locationInSource
        lines should have size 4
        lines shouldBe List(
          ("1", "            var a = 0"),
          ("2", "for(var i = x; i < 5; i++)"),
          ("", "               ~~~~~"),
          ("3", "            a++")
        )
      }

      test("With color") {
        val messageFormatter = getMessageFormatter(useColor = true, tabWidth = 6, message = Some(message))
        val (nums, lines) = messageFormatter.locationInSource.unzip
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
      val message = createMessage(messageType = MessageType.Warning, pos = PositionWithSource(3, 3, 3, 6, source = Some(source)))

      test("Without color") {
        val messageFormatter = getMessageFormatter(useColor = false, tabWidth = 6, message = Some(message))
        val lines = messageFormatter.locationInSource
        lines should have size 4
        lines shouldBe List(
          ("1", "            var a = 0"),
          ("2", "for(var i = x; i < 5; i++)"),
          ("3", "            a++"),
          ("", "            ~~~")
        )
      }

      test("With color") {
        val messageFormatter = getMessageFormatter(useColor = true, tabWidth = 6, message = Some(message))
        val (nums, lines) = messageFormatter.locationInSource.unzip
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
    var errorPos = PositionWithSource(2, 3, 3, 6, source = Some(source))
    var message = createMessage(messageType = MessageType.Warning, pos = errorPos)

    var messageFormatter = getMessageFormatter(useColor = false, message = Some(message))
    var lines = messageFormatter.locationInSource
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
    errorPos = PositionWithSource(2, 1, 3, 28, source = Some(source))
    message = createMessage(messageType = MessageType.Warning, pos = errorPos)

    messageFormatter = getMessageFormatter(useColor = false, message = Some(message))
    lines = messageFormatter.locationInSource
    lines should have size 5
    lines shouldBe List(
      ("1", "else"),
      ("2", "  return // res: F1002"), // error indicator should continue over the whole line
      ("", "  ~~~~~~~~~~~~~~~~~~~~"),
      ("3", "  println(a) // res: F1000"),
      ("", "  ~~~~~~~~~~~~~~~~~~~~~~~~")
    )

    // try last case with colors as well
    messageFormatter = getMessageFormatter(useColor = true, message = Some(message))
    lines = messageFormatter.locationInSource
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


  private def getMessageFormatter(
    useColor: Boolean,
    contextSize: Int = DefaultContextSize,
    tabWidth: Int = DefaultTabWidth,
    syntaxHighlighter: SyntaxHighlighter = mockedSyntaxHighlighter,
    message: Option[CompilerMessage] = None,
    formatting: Option[Formatting] = None
  ): MessageFormatter = {
    val formatter = createMockFormatter(useColor = useColor, syntaxHighlighter = syntaxHighlighter, formatting = formatting)
    MessageFormatter(formatter, TabReplacer(tabWidth), contextSize) use { messageFormatter =>
      message ifDefined { messageFormatter.setMessage }
    }
  }


  private def createMessage(
    messageType: MessageType = MessageType.Error,
    errorLetters: String = "ABC",
    codeNum: Int = 0,
    pos: Positioned = NoPosition,
    mess: String = "ABC"
  ): CompilerMessage = {
    new CompilerMessage(messageType, errorLetters, messageType.typeCode, codeNum, pos) {override def message = mess }
  }

  // This mocked syntax highlighter just returns the input again
  private def mockedSyntaxHighlighter: SyntaxHighlighter =
    mock[SyntaxHighlighter] use { syntaxHighlighter => syntaxHighlighter.apply(*).forwardsArg(0) }


}