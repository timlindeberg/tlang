package tlang.compiler.output

import tlang.compiler.messages.{CompilerMessage, CompilerMessages, MessageTesting, MessageType}
import tlang.compiler.utils.TLangSyntaxHighlighter
import tlang.formatting.{Formatter, PrettyFormatting, SimpleFormatting}
import tlang.formatting.textformatters.TabReplacer
import tlang.testutils.UnitSpec
import tlang.utils._

class ErrorMessageOutputSpec extends UnitSpec with MessageTesting {

  private val PrettyFormatter = Formatter(PrettyFormatting, TLangSyntaxHighlighter(PrettyFormatting))
  private val SimpleFormatter = Formatter(SimpleFormatting)

  private val path = "src/a/path/to/the/File.t"
  private val source = mock[Source]
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
  source.description(*, *) returns path
  source.description returns path

  private val aPos = Position(1, 5, 1, 6, source = Some(source))
  private val iLessThanPos = Position(6, 16, 6, 21, source = Some(source))
  private val dPlusPos = Position(10, 3, 10, 6, source = Some(source))
  private val multipleLinesPos = Position(4, 1, 6, 8, source = Some(source))


  it should "output an error message" in {
    val messages = List(
      createMessage(
        messageType = MessageType.Error,
        errorLetters = "ABC",
        codeNum = 123,
        message = "An error message",
        pos = aPos
      )
    )

    testOutputSnapshots(makeErrorMessageOutput(messages))
  }

  it should "output a warning message" in {
    val messages = List(
      createMessage(
        messageType = MessageType.Warning,
        errorLetters = "DEF",
        codeNum = 456,
        message = "A warning message",
        pos = aPos
      )
    )
    testOutputSnapshots(makeErrorMessageOutput(messages))
  }


  it should "output multiple error messages" in {
    val messages = List(
      createMessage(
        messageType = MessageType.Error,
        errorLetters = "ABC",
        codeNum = 123,
        message = "An error message",
        pos = multipleLinesPos
      ),
      createMessage(
        messageType = MessageType.Error,
        errorLetters = "DEF",
        codeNum = 456,
        message = "Another error message",
        pos = dPlusPos
      )
    )

    testOutputSnapshots(makeErrorMessageOutput(messages))
  }

  it should "output multiple error and warning messages" in {
    val messages = List(
      createMessage(
        messageType = MessageType.Error,
        errorLetters = "ABC",
        codeNum = 123,
        message = "A",
        pos = aPos
      ),
      createMessage(
        messageType = MessageType.Fatal,
        errorLetters = "DEF",
        codeNum = 456,
        message = "B",
        pos = dPlusPos
      ),
      createMessage(
        messageType = MessageType.Warning,
        errorLetters = "GHI",
        codeNum = 789,
        message = "C",
        pos = iLessThanPos
      ),
      createMessage(
        messageType = MessageType.Warning,
        errorLetters = "JKL",
        codeNum = 112,
        message = "D",
        pos = multipleLinesPos
      )
    )
    testOutputSnapshots(makeErrorMessageOutput(messages))
  }

  it should "output error messages with context size" in {
    val messages = List(
      createMessage(
        messageType = MessageType.Error,
        errorLetters = "ABC",
        codeNum = 123,
        message = "A",
        pos = aPos
      ),
      createMessage(
        messageType = MessageType.Error,
        errorLetters = "DEF",
        codeNum = 456,
        message = "B",
        pos = dPlusPos
      )
    )
    test("0") {
      testOutputSnapshots(makeErrorMessageOutput(messages, 0))
    }
    test("1") {
      testOutputSnapshots(makeErrorMessageOutput( messages, 1))
    }
    test("10") {
      testOutputSnapshots(makeErrorMessageOutput(messages, 10))
    }
  }

  it should "output error messages with notes" in {
    val messages = List(
      createMessage(
        messageType = MessageType.Error,
        errorLetters = "ABC",
        codeNum = 123,
        pos = aPos,
        message = "A message with a note",
        notes = List(
          createMessage(
            messageType = MessageType.Note,
            errorLetters = "ABC",
            codeNum = 123,
            message = "A note",
            pos = dPlusPos
          )
        )
      )
    )
    testOutputSnapshots(makeErrorMessageOutput(messages))
  }


  it should "output only maximum errors" in {
    val messages = List(
      createMessage(
        messageType = MessageType.Error,
        errorLetters = "ABC",
        codeNum = 123,
        pos = aPos,
        message = "Message 1"
      ),
      createMessage(
        messageType = MessageType.Error,
        errorLetters = "DEF",
        codeNum = 234,
        pos = dPlusPos,
        message = "Message 2"
      ),
      createMessage(
        messageType = MessageType.Error,
        errorLetters = "GHI",
        codeNum = 345,
        pos = iLessThanPos,
        message = "Message 2"
      ),
      createMessage(
        messageType = MessageType.Error,
        errorLetters = "JKL",
        codeNum = 456,
        pos = multipleLinesPos,
        message = "Message 2"
      )
    )
    testOutputSnapshots(makeErrorMessageOutput(messages, maxErrors = 3))
  }

  it should "show errors with no source" in {
    val messages = List(
      createMessage(
        messageType = MessageType.Error,
        errorLetters = "ABC",
        codeNum = 123,
        pos = Position(1, 2, 3, 4, None),
        message = "A message with no source"
      )
    )
    testOutputSnapshots(makeErrorMessageOutput(messages))
  }

  it should "not output with no errors" in {
    val messages = List()
    val output = makeErrorMessageOutput(messages)
    output.pretty(SimpleFormatter) shouldBe ""
    val json = output.json
    json("compilationWarnings") shouldBe Nil
    json("compilationErrors") shouldBe Nil
  }

  private def testOutputSnapshots(output: ErrorMessageOutput): Unit = {
    test("using pretty formatting") { output.pretty(PrettyFormatter) should matchSnapshot }
    test("using simple formatting") { output.pretty(SimpleFormatter) should matchSnapshot }
    test("as JSON") { JSON(output.json) should matchSnapshot }
  }

  private def makeErrorMessageOutput(
    compilerMessages: Traversable[CompilerMessage],
    messageContextSize: Int = 3,
    maxErrors: Int = 100
  ): ErrorMessageOutput = {
    val cm = CompilerMessages(maxErrors)
    cm ++= compilerMessages
    ErrorMessageOutput(cm, TabReplacer(2), messageContextSize)
  }

}
