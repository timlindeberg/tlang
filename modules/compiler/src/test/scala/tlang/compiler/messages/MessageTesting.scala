package tlang
package compiler
package messages

import tlang.utils.{NoPosition, Positioned}

trait MessageTesting {

  def createMessage(
    messageType: MessageType = MessageType.Error,
    errorLetters: String = "ABC",
    codeNum: Int = 0,
    pos: Positioned = NoPosition,
    message: String = "ABC",
    valid: Boolean = true,
    notes: List[CompilerMessage] = Nil
  ): CompilerMessage = {
    val mess = message
    val ex = notes
    new CompilerMessage(messageType, errorLetters, messageType.typeCode, codeNum, pos) {
      override def message: String = mess
      override def isValid: Boolean = valid
      override def notes: List[CompilerMessage] = ex
    }
  }

}
