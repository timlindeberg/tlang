package tlang.compiler.messages

import tlang.formatting.Colors.Color
import tlang.formatting.{Colors, Formatter}
import tlang.utils.Positioned


object CompilerMessage {
  val ErrorName = "$ERROR"
}
abstract class CompilerMessage(
  val messageType: MessageType,
  val errorLetters: String,
  val typeCode: String,
  val codeNum: Int,
  val pos: Positioned) {
  mess =>

  val code: String = errorLetters + typeCode + leftPadCode(codeNum)

  def message: String

  def isValid = true

  def notes: List[CompilerMessage] = Nil

  def copy(
    messageType: MessageType = this.messageType,
    errorLetters: String = this.errorLetters,
    typeCode: String = this.typeCode,
    codeNum: Int = this.codeNum,
    pos: Positioned = this.pos,
    notes: List[CompilerMessage] = this.notes
  ): CompilerMessage = {
    new CompilerMessage(messageType, errorLetters, typeCode, codeNum, pos) {
      override def message: String = mess.message
      override def notes: List[CompilerMessage] = mess.notes
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
    case -1                        => ""
    case x if x >= 0 && x < 10     => "00" + x
    case x if x >= 10 && x < 100   => "0" + x
    case x if x >= 100 && x < 1000 => "" + x
  }
}

abstract class WarningMessage(override val errorLetters: String, override val codeNum: Int, override val pos: Positioned)
  extends CompilerMessage(MessageType.Warning, errorLetters, MessageType.Warning.typeCode, codeNum, pos) with Product

abstract class ErrorMessage(override val errorLetters: String, override val codeNum: Int, override val pos: Positioned)
  extends CompilerMessage(MessageType.Error, errorLetters, MessageType.Error.typeCode, codeNum, pos) with Product

abstract class FatalMessage(override val errorLetters: String, override val codeNum: Int, override val pos: Positioned)
  extends CompilerMessage(MessageType.Fatal, errorLetters, MessageType.Fatal.typeCode, codeNum, pos) with Product

abstract class ExtraMessage(override val pos: Positioned)
  extends CompilerMessage(MessageType.Note, "", MessageType.Note.typeCode, -1, pos) with Product


sealed abstract class MessageType(val typeCode: String, private val _color: Color) {
  def color(implicit formatter: Formatter): Color = formatter.translate(_color)
  def name: String = getClass.getSimpleName.dropRight(1)
}
object MessageType {
  case object Warning extends MessageType("1", Colors.Yellow)
  case object Error extends MessageType("2", Colors.Red)
  case object Fatal extends MessageType("3", Colors.Red)
  case object Note extends MessageType( "", Colors.Blue)
}
