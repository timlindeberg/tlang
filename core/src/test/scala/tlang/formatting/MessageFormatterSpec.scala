package tlang.formatting

import java.io.File

import tlang.compiler.error._
import tlang.testutils.UnitSpec
import tlang.utils.Extensions._
import tlang.utils._

class MessageFormatterSpec extends UnitSpec {


  val DefaultContextSize = 2
  val DefaultTabWidth    = 2

  val TestFileSource = FileSource(new File("core/src/test/resources/positions/ParserPositions.t"))

  def getMessageFormatter(
    useColor: Boolean = true,
    contextSize: Int = DefaultContextSize,
    tabWidth: Int = DefaultTabWidth,
    syntaxHighlighter: SyntaxHighlighter = mock[SyntaxHighlighter],
    message: Option[CompilerMessage] = None,
    formatting: Option[Formatting] = None
  ): MessageFormatter = {
    val formatter = createMockFormatter(useColor = useColor, syntaxHighlighter = syntaxHighlighter, formatting = formatting)
    MessageFormatter(formatter, contextSize, tabWidth) use { messageFormatter =>
      message ifDefined { messageFormatter.setMessage }
    }
  }

  behavior of "A message formatter"

  def createMessage(
    messageType: MessageType = MessageType.Error,
    errorLetters: String = "ABC",
    codeNum: Int = 0,
    pos: Positioned = Position.NoPos,
    mess: String = "ABC"
  ) = {
    new CompilerMessage(messageType, errorLetters, codeNum, pos) {override def message = mess }
  }


  it should "use correct color depending on the messagetype" in {
    var messageFormatter = getMessageFormatter()

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
    var messageFormatter = getMessageFormatter()
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
    var messageFormatter = getMessageFormatter()
    val message = createMessage(pos = Position(1, 10, 100, 1000))
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
    (source.lines _).expects().twice().returning(IndexedSeq())
    (source.description _)
      .expects(formattingWithColor)
      .returning(s"core/src/test/resources/positions/\u001b[1;35mParserPositions.t\u001b[0m")

    (source.description _)
      .expects(formattingWithoutColor)
      .returning(s"core/src/test/resources/positions/ParserPositions.t")

    var messageFormatter = getMessageFormatter(formatting = Some(formattingWithColor))

    val posWithFile = Position(1, 10, 100, 1000, source = Some(source))
    messageFormatter setMessage createMessage(pos = posWithFile)

    messageFormatter.sourceDescription should matchWithAnsi(
      s"\u001b[1;35m1\u001b[0m:\u001b[1;35m10\u001b[0m core/src/test/resources/positions/\u001b[1;35mParserPositions.t\u001b[0m"
    )

    // Without color
    messageFormatter = getMessageFormatter(formatting = Some(formattingWithoutColor))
    messageFormatter setMessage createMessage(pos = posWithFile)
    messageFormatter.sourceDescription should matchWithAnsi(s"1:10 core/src/test/resources/positions/ParserPositions.t")
  }

  it should "show the location with color" in {
    import Colors._

    val source = mock[Source]
    (source.lines _).expects().returning(IndexedSeq("for(var i = x; i < 5; i++)"))

    // the error is in "i < 5"
    val errorPos = Position(1, 16, 1, 21, source = Some(source))

    // This is not really what the syntax highlighter should return but theres not really
    // any point in mocking that here
    val syntaxHighlighter = mock[SyntaxHighlighter]
    (syntaxHighlighter.apply(_: String, _: Marking))
      .expects("for(var i = x; i < 5; i++)", Marking(Position(1, 16, 1, 21), Bold + Underlined + Red, 1))
      .returning("for(var i = x; \u001b[1;4;31mi < 5\u001b[0m; i++)")


    val messageFormatter = getMessageFormatter(contextSize = 0, syntaxHighlighter = syntaxHighlighter)

    messageFormatter setMessage createMessage(pos = errorPos)

    val lines = messageFormatter.locationInSource
    lines should have size 1

    val (num, line) = lines(0)
    num should matchWithAnsi("\u001b[35m1\u001b[0m")
    line should matchWithAnsi("for(var i = x; \u001b[1;4;31mi < 5\u001b[0m; i++)")
  }

  it should "show the location in the file without colors" in {
    val source = mock[Source]
    (source.lines _).expects().returning(IndexedSeq("for(var i = x; i < 5; i++)"))
    // the error is in "i < 5"
    val errorPos = Position(1, 16, 1, 21, source = Some(source))

    val messageFormatter = getMessageFormatter(contextSize = 1, useColor = false)

    messageFormatter setMessage createMessage(messageType = MessageType.Warning, pos = errorPos)

    val lines = messageFormatter.locationInSource
    lines should have size 2

    val (num1, line1) = lines(0)
    num1 should matchWithAnsi("1")
    line1 should matchWithAnsi("for(var i = x; i < 5; i++)")

    val (num2, line2) = lines(1)
    num2 should matchWithAnsi("")
    line2 should matchWithAnsi("               ‾‾‾‾‾")
  }

  it should "handle different context sizes" in {
    val source = mock[Source]
    (source.lines _).expects().atLeastOnce().returning(IndexedSeq(
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
    ))
    // the error is in "i < 5"
    var message = createMessage(pos = Position(6, 16, 6, 21, source = Some(source)))

    // No context lines
    var messageFormatter = getMessageFormatter(contextSize = 0, useColor = false, message = Some(message))
    var lines = messageFormatter.locationInSource
    lines should have size 2
    lines shouldBe List(
      ("6", "for(var i = x; i < 5; i++)"),
      ("", "               ‾‾‾‾‾")
    )

    // Context size 1
    messageFormatter = getMessageFormatter(contextSize = 1, useColor = false, message = Some(message))
    lines = messageFormatter.locationInSource
    lines should have size 4
    lines shouldBe List(
      ("5", "var e = 0"),
      ("6", "for(var i = x; i < 5; i++)"),
      ("", "               ‾‾‾‾‾"),
      ("7", "    a++")
    )

    // Context size 2
    messageFormatter = getMessageFormatter(contextSize = 2, useColor = false, message = Some(message))
    lines = messageFormatter.locationInSource
    lines should have size 6
    lines shouldBe List(
      ("4", "var d = 0"),
      ("5", "var e = 0"),
      ("6", "for(var i = x; i < 5; i++)"),
      ("", "               ‾‾‾‾‾"),
      ("7", "    a++"),
      ("8", "    b++")
    )

    // Context size 5
    messageFormatter = getMessageFormatter(contextSize = 5, useColor = false, message = Some(message))
    lines = messageFormatter.locationInSource
    lines should have size 12
    lines shouldBe List(
      ("1", "var a = 0"),
      ("2", "var b = 0"),
      ("3", "var c = 0"),
      ("4", "var d = 0"),
      ("5", "var e = 0"),
      ("6", "for(var i = x; i < 5; i++)"),
      ("", "               ‾‾‾‾‾"),
      ("7", "    a++"),
      ("8", "    b++"),
      ("9", "    c++"),
      ("10", "    d++"),
      ("11", "    e++")
    )

    // Context larger than number of lines
    messageFormatter = getMessageFormatter(contextSize = 25, useColor = false, message = Some(message))
    lines = messageFormatter.locationInSource
    lines should have size 12
    lines shouldBe List(
      ("1", "var a = 0"),
      ("2", "var b = 0"),
      ("3", "var c = 0"),
      ("4", "var d = 0"),
      ("5", "var e = 0"),
      ("6", "for(var i = x; i < 5; i++)"),
      ("", "               ‾‾‾‾‾"),
      ("7", "    a++"),
      ("8", "    b++"),
      ("9", "    c++"),
      ("10", "    d++"),
      ("11", "    e++")
    )

    // Error at first line, should only have context lines below
    message = createMessage(pos = Position(1, 5, 1, 6, source = Some(source)))
    messageFormatter = getMessageFormatter(contextSize = 2, useColor = false, message = Some(message))
    lines = messageFormatter.locationInSource
    lines should have size 4
    lines shouldBe List(
      ("1", "var a = 0"),
      ("", "    ‾"),
      ("2", "var b = 0"),
      ("3", "var c = 0")
    )

    // Error at second line, should only have one context line above
    message = createMessage(pos = Position(2, 5, 2, 6, source = Some(source)))
    messageFormatter = getMessageFormatter(contextSize = 2, useColor = false, message = Some(message))
    lines = messageFormatter.locationInSource
    lines should have size 5
    lines shouldBe List(
      ("1", "var a = 0"),
      ("2", "var b = 0"),
      ("", "    ‾"),
      ("3", "var c = 0"),
      ("4", "var d = 0")
    )

    // Error at last line, should only havecontext lines above
    // Indentation should also be trimmed for these lines
    message = createMessage(pos = Position(11, 3, 11, 4, source = Some(source)))
    messageFormatter = getMessageFormatter(contextSize = 2, useColor = false, message = Some(message))
    lines = messageFormatter.locationInSource
    lines should have size 4
    lines shouldBe List(
      ("9", "c++"),
      ("10", "d++"),
      ("11", "e++"),
      ("", "‾")
    )
  }

  it should "trim indentation of location" in {
    val source = mock[Source]
    (source.lines _).expects().atLeastOnce().returning(IndexedSeq(
      "\t\t\tvar a = 0",
      "\t\t\tfor(var i = x; i < 5; i++)",
      "\t\t\t\ta++"
    ))
    // the error is in "i < 5"
    val errorPos = Position(2, 19, 2, 24, source = Some(source))
    val message = createMessage(messageType = MessageType.Warning, pos = errorPos)

    // It should trim indentation correctly without color

    var messageFormatter = getMessageFormatter(useColor = false, message = Some(message))
    var lines = messageFormatter.locationInSource
    lines should have size 4
    lines shouldBe List(
      ("1", "var a = 0"),
      ("2", "for(var i = x; i < 5; i++)"),
      ("", "               ‾‾‾‾‾"),
      ("3", "  a++")
    )

    // And with color

    import Colors._

    // This mocked syntax highlighter only makes the text bold and underlines the position
    // The test makes sure that ansi colors are not trimmed
    val syntaxHighlighter = mock[SyntaxHighlighter]
    val underlineColor = Bold + Underlined + Yellow
    val adjustedPosition = Position(2, 16, 2, 21)
    (syntaxHighlighter.apply(_: String, _: Marking))
      .expects(
        "var a = 0",
        Marking(adjustedPosition, underlineColor, 1)
      )
      .returning("\u001b[1mvar a = 0\u001b[0m")
    (syntaxHighlighter.apply(_: String, _: Marking))
      .expects(
        "for(var i = x; i < 5; i++)",
        Marking(adjustedPosition, underlineColor, 2)
      )
      .returning("\u001b[1mfor(var i = x; \u001b[1;4;31mi < 5\u001b[1m; i++\u001b[0m")
    (syntaxHighlighter.apply(_: String, _: Marking))
      .expects(
        "  a++",
        Marking(adjustedPosition, underlineColor, 3)
      )
      .returning("\u001b[1m  a++\u001b[0m")

    messageFormatter = getMessageFormatter(useColor = true, message = Some(message), syntaxHighlighter = syntaxHighlighter)
    lines = messageFormatter.locationInSource
    lines should have size 3
    val (nums, texts) = lines.unzip
    nums should allMatchWithAnsi(
      "\u001b[35m1\u001b[0m",
      "\u001b[35m2\u001b[0m",
      "\u001b[35m3\u001b[0m"
    )
    texts should allMatchWithAnsi(
      "\u001b[1mvar a = 0\u001b[0m",
      "\u001b[1mfor(var i = x; \u001b[1;4;31mi < 5\u001b[1m; i++\u001b[0m",
      "\u001b[1m  a++\u001b[0m"
    )
  }

  it should "replace tabs in location" in {
    val source = mock[Source]
    (source.lines _).expects().atLeastOnce().returning(IndexedSeq(
      "\t\tvar a = 0",
      "for(var i = x; i < 5; i++)",
      "\t\ta++"
    ))

    // the error is in "i < 5"
    var message = createMessage(messageType = MessageType.Warning, pos = Position(2, 16, 2, 21, source = Some(source)))

    var messageFormatter = getMessageFormatter(useColor = false, tabWidth = 2, message = Some(message))
    var lines = messageFormatter.locationInSource
    lines should have size 4
    lines shouldBe List(
      ("1", "    var a = 0"),
      ("2", "for(var i = x; i < 5; i++)"),
      ("", "               ‾‾‾‾‾"),
      ("3", "    a++")
    )

    messageFormatter = getMessageFormatter(useColor = false, tabWidth = 6, message = Some(message))
    lines = messageFormatter.locationInSource
    lines should have size 4
    lines shouldBe List(
      ("1", "            var a = 0"),
      ("2", "for(var i = x; i < 5; i++)"),
      ("", "               ‾‾‾‾‾"),
      ("3", "            a++")
    )

    // error in a++
    message = createMessage(messageType = MessageType.Warning, pos = Position(3, 3, 3, 6, source = Some(source)))

    messageFormatter = getMessageFormatter(useColor = false, tabWidth = 6, message = Some(message))
    lines = messageFormatter.locationInSource
    lines should have size 4
    lines shouldBe List(
      ("1", "            var a = 0"),
      ("2", "for(var i = x; i < 5; i++)"),
      ("3", "            a++"),
      ("", "            ‾‾‾")
    )
  }

  it should "mark errors over multiple lines" in {
    var source = mock[Source]
    (source.lines _).expects().atLeastOnce().returning(IndexedSeq(
      "for(var i = x; i < 5; i++)",
      "\t\ta++ // abcdef",
      "\t\tb++"
    ))

    // the error is from "a" all the way to the end
    var errorPos = Position(2, 3, 3, 6, source = Some(source))
    var message = createMessage(messageType = MessageType.Warning, pos = errorPos)

    var messageFormatter = getMessageFormatter(useColor = false, message = Some(message))
    var lines = messageFormatter.locationInSource
    lines should have size 5
    lines shouldBe List(
      ("1", "for(var i = x; i < 5; i++)"),
      ("2", "    a++ // abcdef"), // error indicator should continue over the whole line
      ("", "    ‾‾‾‾‾‾‾‾‾‾‾‾‾"),
      ("3", "    b++"),
      ("", "‾‾‾‾‾‾‾")
    )

    source = mock[Source]
    (source.lines _).expects().atLeastOnce().returning(IndexedSeq(
      "\t\telse",
      "\t\t\treturn // res: F1002",
      "\t\t\tprintln(a) // res: F1000"
    ))
    errorPos = Position(2, 1, 3, 28, source = Some(source))
    message = createMessage(messageType = MessageType.Warning, pos = errorPos)

    messageFormatter = getMessageFormatter(useColor = false, message = Some(message))
    lines = messageFormatter.locationInSource
    lines should have size 5
    lines shouldBe List(
      ("1", "else"),
      ("2", "  return // res: F1002"), // error indicator should continue over the whole line
      ("", "‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾"),
      ("3", "  println(a) // res: F1000"),
      ("", "‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾")
    )

    // try last case with colors as well

    import Colors._
    val adjustedPosition = Position(2, 1, 3, 27)
    val syntaxHighlighter = mock[SyntaxHighlighter]
    val underlineColor = Bold + Underlined + Yellow

    (syntaxHighlighter.apply(_: String, _: Marking))
      .expects(
        "else",
        Marking(adjustedPosition, underlineColor, 1)
      )
      .returning("\u001b[1melse\u001b[0m")

    (syntaxHighlighter.apply(_: String, _: Marking))
      .expects(
        "  return // res: F1002",
        Marking(adjustedPosition, underlineColor, 2)
      )
      .returning("\u001b[1;4;33m  return // res: F1002\u001b[0m")

    (syntaxHighlighter.apply(_: String, _: Marking))
      .expects(
        "  println(a) // res: F1000",
        Marking(adjustedPosition, underlineColor, 3)
      )
      .returning("\u001b[1;4;33m  println(a) // res: F1000\u001b[0m")

    messageFormatter = getMessageFormatter(useColor = true, message = Some(message), syntaxHighlighter = syntaxHighlighter)
    lines = messageFormatter.locationInSource
    lines should have size 3
    val (nums, texts) = lines.unzip
    nums should allMatchWithAnsi(
      "\u001b[35m1\u001b[0m",
      "\u001b[35m2\u001b[0m",
      "\u001b[35m3\u001b[0m"
    )
    texts should allMatchWithAnsi(
      "\u001b[1melse\u001b[0m",
      "\u001b[1;4;33m  return // res: F1002\u001b[0m",
      "\u001b[1;4;33m  println(a) // res: F1000\u001b[0m"
    )
  }

}