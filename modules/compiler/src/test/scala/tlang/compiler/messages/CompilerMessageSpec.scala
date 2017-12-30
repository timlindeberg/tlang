package tlang.compiler.messages

import org.mockito.ArgumentMatchers
import tlang.compiler.ast.Trees.{ClassID, IntLit, Plus, VariableID}
import tlang.formatting.Formatter
import tlang.testutils.UnitSpec
import tlang.utils.{NoPosition, Position, Positioned}

class CompilerMessageSpec extends UnitSpec {

  behavior of "Compiler messages"

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
    compilerMessages += createMessage(messageType = MessageType.Warning, errorLetters = "A", codeNum = 1, pos = NoPosition, message = "WARNING1")
    compilerMessages += createMessage(messageType = MessageType.Warning, errorLetters = "B", codeNum = 2, pos = NoPosition, message = "WARNING2")
    compilerMessages += createMessage(messageType = MessageType.Warning, errorLetters = "C", codeNum = 3, pos = NoPosition, message = "WARNING3")

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

    compilerMessages += createMessage(messageType = MessageType.Error, pos = Plus(IntLit(1), IntLit(1)), valid = false)
    compilerMessages += createMessage(messageType = MessageType.Error, pos = VariableID("Id"), valid = false)
    compilerMessages += createMessage(messageType = MessageType.Error, pos = ClassID("Id"), valid = false)

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

    val formatter = testFormatter(
      width = 40,
      useColor = false,
      asciiOnly = false,
      wordWrapper = mockedWordWrapperReturningSplitLines,
      truncator = mockedTruncatorReturningSameLine
    )

    test("two errors with valid positions") {
      val messageFormatter = mock[MessageFormatter]

      messageFormatter.hasValidPosition returns true
      messageFormatter.sourceDescription returns(
        "1:3 from/a/very/cool/File.t",
        "54:1 from/another/cool/File.t"
      )

      messageFormatter.prefix returns(
        "Error A2123",
        "Error B2321"
      )
      messageFormatter.locationInSource returns(
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
        """|╒══════════════════════════════════════╕
           |│         There were 2 errors.         │
           |╞══════════════════════════════════════╡
           |│ 1:3 from/a/very/cool/File.t          │
           |│ Error A2123 There was an error!!!    │
           |├───┬──────────────────────────────────┤
           |│ 1 │ ABCDEFFGHIJKLMNOPQRSTUVXYZ       │
           |│   │   ~~~~~                          │
           |│ 2 │ ABCDEFFGHIJKLMNOPQRSTUVXYZ       │
           |│ 3 │ ABCDEFFGHIJKLMNOPQRSTUVXYZ       │
           |├───┴──────────────────────────────────┤
           |│ 54:1 from/another/cool/File.t        │
           |│ Error B2321 Moar errors!!!           │
           |├────┬─────────────────────────────────┤
           |│ 53 │ Line 1Line 1Line 1              │
           |│ 54 │ Line 2Line 2Line 2              │
           |│    │ ~~~~                            │
           |│ 55 │ Line 3Line 3Line 3              │
           |└────┴─────────────────────────────────┘""".stripMargin
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

      val compilerMessages = createCompilerMessages(
        width = 40,
        useColor = false,
        messageFormatter = messageFormatter,
        formatter = Some(formatter)
      )
      compilerMessages += warning
      compilerMessages.formatMessages(MessageType.Warning) shouldBe
        """|╒══════════════════════════════════════╕
           |│         There was 1 warning.         │
           |╞══════════════════════════════════════╡
           |│ Warning C1001 Here be a warning!!!   │
           |└──────────────────────────────────────┘""".stripMargin
    }
  }

  it should "truncate long lines" in {
    val messageFormatter = mock[MessageFormatter]

    messageFormatter.hasValidPosition returns true
    messageFormatter.sourceDescription returns "1:3 from/a/very/cool/File.t"

    messageFormatter.prefix returns "Error A2123"
    messageFormatter.locationInSource returns List(
      ("1", "ABCDEFFGHIJKLMNOPQRSTUVXYZABCDEFFG"),
      ("", "  ~~~~~"),
      ("2", "ABCDEFFGHIJKLMNOPQRSTUVXYZ"),
      ("3", "ABCDEFFGHIJKLMNOPQRSTUVXYZ")
    )

    val truncator = mockedTruncatorReturningSameLine
    truncator.apply(ArgumentMatchers.eq("ABCDEFFGHIJKLMNOPQRSTUVXYZABCDEFFG"), *) returns "ABCDEFFGHIJKLMNOPQRSTUVXYZABC..."
    val formatter = testFormatter(
      width = 40,
      useColor = false,
      asciiOnly = false,
      wordWrapper = mockedWordWrapperReturningSplitLines,
      truncator = truncator
    )
    val compilerMessages = createCompilerMessages(
      width = 40,
      useColor = false,
      messageFormatter = messageFormatter,
      formatter = Some(formatter)
    )

    compilerMessages += createMessage(
      MessageType.Error,
      errorLetters = "A",
      codeNum = 123,
      pos = Position(1, 3, 1, 8),
      message = "There was an error!!!"
    )
    compilerMessages.formatMessages(MessageType.Error) shouldBe
      """|╒══════════════════════════════════════╕
         |│          There was 1 error.          │
         |╞══════════════════════════════════════╡
         |│ 1:3 from/a/very/cool/File.t          │
         |│ Error A2123 There was an error!!!    │
         |├───┬──────────────────────────────────┤
         |│ 1 │ ABCDEFFGHIJKLMNOPQRSTUVXYZABC... │
         |│   │   ~~~~~                          │
         |│ 2 │ ABCDEFFGHIJKLMNOPQRSTUVXYZ       │
         |│ 3 │ ABCDEFFGHIJKLMNOPQRSTUVXYZ       │
         |└───┴──────────────────────────────────┘""".stripMargin
  }


  it should "include extra info" in {
    val formatter = testFormatter(
      width = 40,
      useColor = false,
      asciiOnly = false,
      wordWrapper = mockedWordWrapperReturningSplitLines,
      truncator = mockedTruncatorReturningSameLine
    )

    val messageFormatter = mock[MessageFormatter]

    messageFormatter.hasValidPosition returns true
    messageFormatter.sourceDescription returns(
      "1:3 from/a/very/cool/File.t",
      "54:1 from/another/cool/File.t"
    )

    messageFormatter.prefix returns(
      "Error A2123",
      "Info"
    )
    messageFormatter.locationInSource returns(
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
      message = "There was an error!!!",
      extraInfo = List(createMessage(
        MessageType.Info,
        errorLetters = "",
        codeNum = -1,
        pos = Position(54, 1, 54, 4),
        message = "Cool extra info for ya!"
      ))
    )
    compilerMessages.formatMessages(MessageType.Error) shouldBe
      """|╒══════════════════════════════════════╕
         |│          There was 1 error.          │
         |╞══════════════════════════════════════╡
         |│ 1:3 from/a/very/cool/File.t          │
         |│ Error A2123 There was an error!!!    │
         |├───┬──────────────────────────────────┤
         |│ 1 │ ABCDEFFGHIJKLMNOPQRSTUVXYZ       │
         |│   │   ~~~~~                          │
         |│ 2 │ ABCDEFFGHIJKLMNOPQRSTUVXYZ       │
         |│ 3 │ ABCDEFFGHIJKLMNOPQRSTUVXYZ       │
         |├───┴──────────────────────────────────┤
         |│ 54:1 from/another/cool/File.t        │
         |│ Info Cool extra info for ya!         │
         |├────┬─────────────────────────────────┤
         |│ 53 │ Line 1Line 1Line 1              │
         |│ 54 │ Line 2Line 2Line 2              │
         |│    │ ~~~~                            │
         |│ 55 │ Line 3Line 3Line 3              │
         |└────┴─────────────────────────────────┘""".stripMargin
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
    val f = formatter.getOrElse(testFormatter(useColor = useColor, width = width))
    CompilerMessages(f, messageFormatter, maxErrors, warningIsError, suppressWarnings)
  }


  private def createMessage(
    messageType: MessageType = MessageType.Error,
    errorLetters: String = "ABC",
    codeNum: Int = 0,
    pos: Positioned = NoPosition,
    message: String = "ABC",
    valid: Boolean = true,
    extraInfo: List[CompilerMessage] = Nil
  ): CompilerMessage = {
    val mess = message
    val ex = extraInfo
    new CompilerMessage(messageType, errorLetters, messageType.typeCode, codeNum, pos) {
      override def message = mess
      override def isValid: Boolean = valid
      override def extraInfo: List[CompilerMessage] = ex
    }
  }

}
