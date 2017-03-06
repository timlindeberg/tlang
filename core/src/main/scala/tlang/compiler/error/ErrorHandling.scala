package tlang.compiler.error

import tlang.compiler.Context
import tlang.compiler.imports.ImportMap
import tlang.compiler.utils.Positioned

/**
  * Created by Tim Lindeberg on 5/13/2016.
  */
trait ErrorHandling {

  var ctx      : Context
  var importMap: ImportMap

  val nameSuggestor = new NameSuggestor

  def report(warning: Warning): Unit = ctx.reporter.report(warning)
  def report(fatal: Fatal): Nothing = {
    ctx.reporter.report(fatal)
    throw new Exception
  }

  implicit class ErrorStringContext(val sc: StringContext) {

    def err(args: Any*): String = {
      val colors = ctx.formatting.colors

      import colors._

      val strings = sc.parts.iterator
      val expressions = args.iterator
      val sb = new StringBuilder(Bold + strings.next)
      while (strings.hasNext) {
        val next = expressions.next
        sb ++= (next match {
          case Suggestion(Some(suggestion)) => err"Did you mean $suggestion?"
          case Suggestion(None)             => ""
          case _                            =>
            var str = next.toString
            str = TemplateNameParser.parseTemplateName(str)
            str = importMap.replaceNames(str)
            s"'$Reset$Magenta$str$Reset$Bold'"
        })
        sb ++= strings.next
      }
      sb.toString + Reset
    }
  }

}

object ErrorMessage {
  val ErrorName = "$ERROR$"

  def unapply(err: ErrorMessage) = Some((err.code, err.message, err.pos))
}


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
