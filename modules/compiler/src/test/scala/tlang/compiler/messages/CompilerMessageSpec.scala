package tlang
package compiler
package messages

import tlang.compiler.ast.Trees.{ClassID, IntLit, Plus, VariableID}
import tlang.testutils.{MessageTesting, UnitSpec}
import tlang.utils.{NoPosition, Position}

class CompilerMessageSpec extends UnitSpec with MessageTesting {

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

  private def createCompilerMessages(
    useColor: Boolean = true,
    maxErrors: Int = -1,
    warningIsError: Boolean = false,
    suppressWarnings: Boolean = false,
    width: Int = 80
  ): CompilerMessages = {
    CompilerMessages(maxErrors, warningIsError, suppressWarnings)
  }
}
