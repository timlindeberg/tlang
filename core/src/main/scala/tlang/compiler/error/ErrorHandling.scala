package tlang.compiler.error

import tlang.compiler.Context
import tlang.compiler.imports.ImportMap
import tlang.utils.Positioned

trait ErrorHandling {

  def ctx: Context
  def importMap: ImportMap

  val nameSuggestor = new NameSuggestor

  def report(warning: Warning): Unit = ctx.reporter.report(warning)
  def report(fatal: Fatal): Nothing = {
    ctx.reporter.report(fatal)
    // Reporter will throw an exception but this is here so the type can be Nothing
    throw new Exception
  }

  implicit class ErrorStringContext(val sc: StringContext) {

    private val colors = ctx.formatting.colors

    import colors._

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
      case Suggestion(Some(suggestion)) => err" Did you mean $suggestion?"
      case Suggestion(None)             => ""
      case any                          =>
        var str = any.toString
        str = TemplateNameParser.parseTemplateName(str)
        str = importMap.replaceNames(str)

        if (colors.isActive) s"$Reset$Magenta$str" else s"'$str'"
    }
  }

}

object ErrorMessage {val ErrorName = "$ERROR" }
abstract class ErrorMessage(val errorNum: Int, val errorLetters: String, val codeNum: Int, val pos: Positioned) {

  val code: String = errorLetters + errorNum + leftPadCode(codeNum)

  def message: String

  override def equals(obj: Any): Boolean = obj match {
    case err: ErrorMessage =>
      err.errorLetters == errorLetters &&
      err.errorNum == errorNum &&
      err.codeNum == codeNum &&
      err.pos.encodedStartPos == pos.encodedStartPos &&
      err.pos.encodedEndPos == pos.encodedEndPos
    case _                 => false
  }

  override val hashCode: Int = 31 * (code.hashCode ^ (31 * (pos.encodedStartPos ^ 31 * pos.encodedEndPos)))

  private def leftPadCode(num: Int): String = num match {
    case x if x >= 0 && x < 10     => "00" + x
    case x if x >= 10 && x < 100   => "0" + x
    case x if x >= 100 && x < 1000 => "" + x
  }
}

abstract class Warning(override val errorLetters: String, override val codeNum: Int, override val pos: Positioned)
  extends ErrorMessage(1, errorLetters, codeNum, pos)

abstract class Error(override val errorLetters: String, override val codeNum: Int, override val pos: Positioned)
  extends ErrorMessage(2, errorLetters, codeNum, pos)

abstract class Fatal(override val errorLetters: String, override val codeNum: Int, override val pos: Positioned)
  extends ErrorMessage(3, errorLetters, codeNum, pos)
