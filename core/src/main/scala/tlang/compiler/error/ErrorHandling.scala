package tlang.compiler.error

import tlang.utils.Positioned
import tlang.utils.formatting.Colors.Color
import tlang.utils.formatting.{ErrorStringContext, Formatting}

trait ErrorHandling {

  // This is a val since we need a stable identifier in order to import the string context
  val errorStringContext: ErrorStringContext

  def reporter: Reporter
  def replaceNames(str: String): String = str

  def report(warning: WarningMessage): Unit = reporter.report(warning)
  def report(fatal: FatalMessage): Nothing = {
    reporter.report(fatal)
    // Reporter will throw an exception but this is here so the type can be Nothing
    throw new Exception
  }

}

trait MessageType {
  def color(formatting: Formatting): Color
  def name: String = getClass.getSimpleName.dropRight(1)
  def code: Int
}
object MessageType {
  case object Warning extends MessageType {
    override def color(formatting: Formatting): Color = formatting.Yellow
    override def code: Int = 1
  }
  case object Error extends MessageType {
    override def color(formatting: Formatting): Color = formatting.Red
    override def code: Int = 2
  }
  case object Fatal extends MessageType {
    override def color(formatting: Formatting): Color = formatting.Red
    override def code: Int = 3
  }
}

object CompilerMessage {val ErrorName = "$ERROR" }
abstract class CompilerMessage(val messageType: MessageType, val errorLetters: String, val codeNum: Int, val pos: Positioned) {

  val code: String = errorLetters + messageType.code + leftPadCode(codeNum)

  def message: String

  override def equals(obj: Any): Boolean = obj match {
    case err: CompilerMessage =>
      err.errorLetters == errorLetters &&
        err.messageType == messageType &&
        err.codeNum == codeNum &&
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
  extends CompilerMessage(MessageType.Warning, errorLetters, codeNum, pos)

abstract class ErrorMessage(override val errorLetters: String, override val codeNum: Int, override val pos: Positioned)
  extends CompilerMessage(MessageType.Error, errorLetters, codeNum, pos)

abstract class FatalMessage(override val errorLetters: String, override val codeNum: Int, override val pos: Positioned)
  extends CompilerMessage(MessageType.Fatal, errorLetters, codeNum, pos)
