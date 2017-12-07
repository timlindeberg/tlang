package tlang.compiler.messages

import tlang.formatting.Colors.Color
import tlang.formatting.Formatting
import tlang.utils.Positioned


object CompilerMessage {val ErrorName = "$ERROR" }
abstract class CompilerMessage(val messageType: MessageType, val errorLetters: String, val typeCode: Int, val codeNum: Int, val pos: Positioned) {
  mess =>

  val code: String = errorLetters + typeCode + leftPadCode(codeNum)

  def message: String

  def copy(
    messageType: MessageType = this.messageType,
    errorLetters: String = this.errorLetters,
    typeCode: Int = this.typeCode,
    codeNum: Int = this.codeNum,
    pos: Positioned = this.pos): CompilerMessage = {
    new CompilerMessage(messageType, errorLetters, typeCode, codeNum, pos) {
      override def message: String = mess.message
    }
  }

  override def equals(obj: Any): Boolean = obj match {
    case err: CompilerMessage =>
      err.errorLetters == errorLetters &&
        err.messageType == messageType &&
        err.codeNum == codeNum &&
        err.typeCode == typeCode &&
        err.pos.encodedStartPos == pos.encodedStartPos &&
        err.pos.encodedEndPos == pos.encodedEndPos
    case _                    => false
  }

  override val hashCode: Int = 31 * (code.hashCode ^ (31 * (pos.encodedStartPos ^ 31 * pos.encodedEndPos)))

  private def leftPadCode(num: Int): String = num match {
    case x if x >= 0 && x < 10     => "00" + x
    case x if x >= 10 && x < 100   => "0" + x
    case x if x >= 100 && x < 1000 => "" + x
  }
}

abstract class WarningMessage(override val errorLetters: String, override val codeNum: Int, override val pos: Positioned)
  extends CompilerMessage(MessageType.Warning, errorLetters, MessageType.Warning.typeCode, codeNum, pos)

abstract class ErrorMessage(override val errorLetters: String, override val codeNum: Int, override val pos: Positioned)
  extends CompilerMessage(MessageType.Error, errorLetters, MessageType.Error.typeCode, codeNum, pos)

abstract class FatalMessage(override val errorLetters: String, override val codeNum: Int, override val pos: Positioned)
  extends CompilerMessage(MessageType.Fatal, errorLetters, MessageType.Fatal.typeCode, codeNum, pos)


trait MessageType {
  def color(formatting: Formatting): Color
  def name: String = getClass.getSimpleName.dropRight(1)
  def typeCode: Int
}
object MessageType {
  case object Warning extends MessageType {
    override def color(formatting: Formatting): Color = formatting.Yellow
    override def typeCode: Int = 1
  }
  case object Error extends MessageType {
    override def color(formatting: Formatting): Color = formatting.Red
    override def typeCode: Int = 2
  }
  case object Fatal extends MessageType {
    override def color(formatting: Formatting): Color = formatting.Red
    override def typeCode: Int = 3
  }
}