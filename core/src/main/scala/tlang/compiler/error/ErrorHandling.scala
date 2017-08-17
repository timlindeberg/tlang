package tlang.compiler.error

import tlang.utils.Positioned
import tlang.utils.formatting.Colors.Color
import tlang.utils.formatting.Formatting

trait ErrorHandling {

  def reporter: Reporter
  def formatting: Formatting
  def replaceNames(str: String): String = str

  val nameSuggestor = new NameSuggestor

  def report(warning: WarningMessage): Unit = reporter.report(warning)
  def report(fatal: FatalMessage): Nothing = {
    reporter.report(fatal)
    // Reporter will throw an exception but this is here so the type can be Nothing
    throw new Exception
  }

  implicit class ErrorStringContext(val sc: StringContext) {

    private val _formatting = formatting

    import _formatting._

    def err(args: Any*): String = {
      val strings = sc.parts.iterator
      val expressions = args.iterator
      val sb = new StringBuilder(Bold + strings.next)
      while (strings.hasNext) {
        sb ++= evaluate(expressions.next)
        sb ++= Reset + Bold + strings.next
      }
      sb ++= Reset
      sb.toString
    }

    private def evaluate(any: Any) = any match {
      case Suggestion(suggestion) => err" Did you mean $suggestion?"
      case any                    =>
        var str = any.toString
        str = TemplateNameParser.parseTemplateName(str)
        str = replaceNames(str)

        if (formatting.useColor) s"$Reset$Magenta$str" else s"'$str'"
    }
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
