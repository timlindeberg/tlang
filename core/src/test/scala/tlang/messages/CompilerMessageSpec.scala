package tlang.messages

import tlang.compiler.analyzer.Symbols.{ClassErrorSymbol, VariableErrorSymbol}
import tlang.compiler.analyzer.Types.TError
import tlang.compiler.ast.Trees.{ClassID, Empty, VariableID}
import tlang.formatting.Formatter
import tlang.testutils.UnitSpec
import tlang.utils.{Position, Positioned}

class CompilerMessageSpec extends UnitSpec {


  it should "hold error messages and warnings" in {
    val compilerMessages = createCompilerMessages()

    compilerMessages += createMessage(messageType = MessageType.Error, errorLetters = "A")
    compilerMessages += createMessage(messageType = MessageType.Error, errorLetters = "B")
    compilerMessages += createMessage(messageType = MessageType.Error, errorLetters = "C")
    compilerMessages += createMessage(messageType = MessageType.Error, errorLetters = "D")
    compilerMessages += createMessage(messageType = MessageType.Error, errorLetters = "E")

    compilerMessages += createMessage(messageType = MessageType.Warning, errorLetters = "A")
    compilerMessages += createMessage(messageType = MessageType.Warning, errorLetters = "B")
    compilerMessages += createMessage(messageType = MessageType.Warning, errorLetters = "C")

    val errors = compilerMessages(MessageType.Error)
    errors should have size 5
    val warnings = compilerMessages(MessageType.Warning)
    warnings should have size 3
  }

  it should "treat fatal messages as errors" in {
    val compilerMessages = createCompilerMessages()
    compilerMessages += createMessage(messageType = MessageType.Error, errorLetters = "A")
    compilerMessages += createMessage(messageType = MessageType.Error, errorLetters = "B")

    compilerMessages += createMessage(messageType = MessageType.Fatal, errorLetters = "C")
    compilerMessages += createMessage(messageType = MessageType.Fatal, errorLetters = "D")

    val errors = compilerMessages(MessageType.Error)
    errors should have size 4
  }

  it should "suppress warnings" in {
    val compilerMessages = createCompilerMessages(suppressWarnings = true)

    compilerMessages += createMessage(messageType = MessageType.Warning, errorLetters = "A")
    compilerMessages += createMessage(messageType = MessageType.Warning, errorLetters = "B")
    compilerMessages += createMessage(messageType = MessageType.Warning, errorLetters = "C")

    val warnings = compilerMessages(MessageType.Warning)
    warnings should be(empty)
  }

  it should "treat warnings as error" in {
    val compilerMessages = createCompilerMessages(warningIsError = true)
    compilerMessages += createMessage(messageType = MessageType.Warning, errorLetters = "A", codeNum = 1, pos = Position.NoPos, message = "WARNING1")
    compilerMessages += createMessage(messageType = MessageType.Warning, errorLetters = "B", codeNum = 2, pos = Position.NoPos, message = "WARNING2")
    compilerMessages += createMessage(messageType = MessageType.Warning, errorLetters = "C", codeNum = 3, pos = Position.NoPos, message = "WARNING3")

    val warnings = compilerMessages(MessageType.Warning)
    warnings should be(empty)

    val errors = compilerMessages(MessageType.Error)
    errors should have size 3

    errors(0).code shouldBe "A1001"
    errors(1).code shouldBe "B1002"
    errors(2).code shouldBe "C1003"
  }

  it should "not hold more than the maximum amount of errors" in {
    val compilerMessages = createCompilerMessages(maxErrors = 2)
    compilerMessages += createMessage(messageType = MessageType.Error, errorLetters = "A")
    compilerMessages += createMessage(messageType = MessageType.Error, errorLetters = "B")
    compilerMessages += createMessage(messageType = MessageType.Error, errorLetters = "C")

    val warnings = compilerMessages(MessageType.Warning)
    warnings should be(empty)

    val errors = compilerMessages(MessageType.Error)
    errors should have size 2
  }

  it should "display headers correctly" in {
    val compilerMessages = createCompilerMessages(useColor = false)
    val compilerMessagesColor = createCompilerMessages(useColor = true)

    compilerMessages.header(MessageType.Warning) shouldBe ""
    compilerMessagesColor.header(MessageType.Error) shouldBe ""

    compilerMessages += createMessage(messageType = MessageType.Error, errorLetters = "A")
    compilerMessages += createMessage(messageType = MessageType.Warning, errorLetters = "A")
    compilerMessagesColor += createMessage(messageType = MessageType.Error, errorLetters = "A")
    compilerMessagesColor += createMessage(messageType = MessageType.Warning, errorLetters = "A")

    compilerMessages.header(MessageType.Error) shouldBe "There was 1 error."
    compilerMessagesColor.header(MessageType.Error) shouldBe "\u001b[1mThere was \u001b[31m1\u001b[0m\u001b[1m error.\u001b[0m"


    compilerMessages.header(MessageType.Warning) shouldBe "There was 1 warning."
    compilerMessagesColor.header(MessageType.Warning) shouldBe "\u001b[1mThere was \u001b[33m1\u001b[0m\u001b[1m warning.\u001b[0m"

    compilerMessages += createMessage(messageType = MessageType.Error, errorLetters = "B")
    compilerMessages += createMessage(messageType = MessageType.Warning, errorLetters = "B")
    compilerMessagesColor += createMessage(messageType = MessageType.Error, errorLetters = "B")
    compilerMessagesColor += createMessage(messageType = MessageType.Warning, errorLetters = "B")

    compilerMessages.header(MessageType.Error) shouldBe "There were 2 errors."
    compilerMessagesColor.header(MessageType.Error) shouldBe "\u001b[1mThere were \u001b[31m2\u001b[0m\u001b[1m errors.\u001b[0m"


    compilerMessages.header(MessageType.Warning) shouldBe "There were 2 warnings."
    compilerMessagesColor.header(MessageType.Warning) shouldBe "\u001b[1mThere were \u001b[33m2\u001b[0m\u001b[1m warnings.\u001b[0m"

    for (i <- 0 until 10) {
      compilerMessages += createMessage(messageType = MessageType.Error, errorLetters = s"$i")
      compilerMessagesColor += createMessage(messageType = MessageType.Error, errorLetters = s"$i")
    }

    compilerMessages.header(MessageType.Error) shouldBe "There were 12 errors."
    compilerMessagesColor.header(MessageType.Error) shouldBe "\u001b[1mThere were \u001b[31m12\u001b[0m\u001b[1m errors.\u001b[0m"
  }

  it should "not add invalid messages" in {
    val compilerMessages = createCompilerMessages()

    compilerMessages += createMessage(messageType = MessageType.Error, pos = Empty().setType(TError))
    compilerMessages += createMessage(messageType = MessageType.Error, pos = VariableID("Id").setSymbol(VariableErrorSymbol))
    compilerMessages += createMessage(messageType = MessageType.Error, pos = ClassID("Id").setSymbol(ClassErrorSymbol))

    compilerMessages(MessageType.Error) should be(empty)
  }

  it should "not add duplicate messages" in {
    val compilerMessages = createCompilerMessages()

    compilerMessages += createMessage(messageType = MessageType.Error, errorLetters = "A", codeNum = 1, pos = Position(0, 0, 0, 0))
    compilerMessages += createMessage(messageType = MessageType.Error, errorLetters = "A", codeNum = 1, pos = Position(0, 0, 0, 0))
    compilerMessages += createMessage(messageType = MessageType.Warning, errorLetters = "A", codeNum = 1, pos = Position(0, 0, 0, 0))
    compilerMessages += createMessage(messageType = MessageType.Error, errorLetters = "AB", codeNum = 1, pos = Position(0, 0, 0, 0))
    compilerMessages += createMessage(messageType = MessageType.Error, errorLetters = "A", codeNum = 2, pos = Position(0, 0, 0, 0))
    compilerMessages += createMessage(messageType = MessageType.Error, errorLetters = "A", codeNum = 1, pos = Position(1, 0, 0, 0))

    compilerMessages(MessageType.Error) should have size 4
    compilerMessages(MessageType.Warning) should have size 1
  }

  it should "format message correctly" in {

    val formatter = createMockFormatter(width = 40, useColor = false, wordWrapper = mockedWordWrapperReturningSameLine)

    test("two errors with valid positions") {
      val messageFormatter = mock[MessageFormatter]

      messageFormatter.hasValidPosition returns true
      messageFormatter.sourceDescription returnsMulti Seq(
        "1:3 from/a/very/cool/File.t",
        "54:1 from/another/cool/File.t"
      )

      messageFormatter.prefix returnsMulti Seq(
        "Error A2123",
        "Error B2321"
      )
      messageFormatter.locationInSource returnsMulti Seq(
        List(
          ("1", "ABCDEFFGHIJKLMNOPQRSTUVXYZ"),
          ("", "  ~~~~~"),
          ("2", "ABCDEFFGHIJKLMNOPQRSTUVXYZ"),
          ("3", "ABCDEFFGHIJKLMNOPQRSTUVXYZ")
        ),
        List(
          ("53", "Line 1Line 1Line 1"),
          ("54", "Line 2Line 2Line 2"),
          ("", "~~~~"),
          ("55", "Line 3Line 3Line 3")
        )
      )

      val compilerMessages = createCompilerMessages(width = 40, useColor = false, messageFormatter = messageFormatter, formatter = Some(formatter))

      compilerMessages += createMessage(
        MessageType.Error,
        errorLetters = "A",
        codeNum = 123,
        pos = Position(1, 3, 1, 8),
        message = "There was an error!!!"
      )
      compilerMessages += createMessage(
        MessageType.Error,
        errorLetters = "B",
        codeNum = 321,
        pos = Position(54, 1, 54, 4),
        message = "Moar errors!!!"
      )
      compilerMessages.formatMessages(MessageType.Error) shouldBe
        """| ====================================== 
           ||         There were 2 errors.         |
           ||======================================|
           || 1:3 from/a/very/cool/File.t          |
           || Error A2123 There was an error!!!    |
           ||--------------------------------------|
           || 1 | ABCDEFFGHIJKLMNOPQRSTUVXYZ       |
           ||   |   ~~~~~                          |
           || 2 | ABCDEFFGHIJKLMNOPQRSTUVXYZ       |
           || 3 | ABCDEFFGHIJKLMNOPQRSTUVXYZ       |
           ||--------------------------------------|
           || 54:1 from/another/cool/File.t        |
           || Error B2321 Moar errors!!!           |
           ||--------------------------------------|
           || 53 | Line 1Line 1Line 1              |
           || 54 | Line 2Line 2Line 2              |
           ||    | ~~~~                            |
           || 55 | Line 3Line 3Line 3              |
           | -------------------------------------- """.stripMargin
    }

    test("one warning with a non valid position") {
      val messageFormatter = mock[MessageFormatter]

      val warning: CompilerMessage = createMessage(
        MessageType.Warning,
        errorLetters = "C",
        codeNum = 1,
        pos = Position(1, 3, 1, 8),
        message = "Here be a warning!!!"
      )

      messageFormatter.hasValidPosition returns false
      messageFormatter.prefix returns "Warning C1001"

      val compilerMessages = createCompilerMessages(width = 40, useColor = false, messageFormatter = messageFormatter, formatter = Some(formatter))
      compilerMessages += warning
      compilerMessages.formatMessages(MessageType.Warning) shouldBe
        """| ====================================== 
           ||         There was 1 warning.         |
           ||======================================|
           || Warning C1001 Here be a warning!!!   |
           | -------------------------------------- """.stripMargin
    }

  }

  private def createCompilerMessages(
    useColor: Boolean = true,
    messageFormatter: MessageFormatter = mock[MessageFormatter],
    formatter: Option[Formatter] = None,
    maxErrors: Int = -1,
    warningIsError: Boolean = false,
    suppressWarnings: Boolean = false,
    width: Int = 80
  ): CompilerMessages = {
    val f = formatter.getOrElse(createMockFormatter(useColor = useColor, width = width))
    CompilerMessages(f, messageFormatter, maxErrors, warningIsError, suppressWarnings)
  }


  private def createMessage(
    messageType: MessageType = MessageType.Error,
    errorLetters: String = "ABC",
    codeNum: Int = 0,
    pos: Positioned = Position.NoPos,
    message: String = "ABC"
  ): CompilerMessage = {
    val mess = message
    new CompilerMessage(messageType, errorLetters, messageType.typeCode, codeNum, pos) {override def message = mess }
  }

}
