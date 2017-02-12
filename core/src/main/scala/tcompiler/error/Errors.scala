package tcompiler.error

import tcompiler.imports.ImportMap
import tcompiler.utils.{Context, Positioned}

import scala.util.matching.Regex

/**
  * Created by Tim Lindeberg on 5/13/2016.
  */
trait Errors {

  val ErrorLetters: String
  var ctx         : Context
  var importMap   : ImportMap

  val QuoteRegex: Regex = """'(.+?)'""".r
  val nameSuggestor     = new NameSuggestor


  def warning(errorCode: Int, msg: String, pos: Positioned): Unit =
    report(errorCode, msg, ErrorLevel.Warning, pos)

  def fatal(errorCode: Int, msg: String, pos: Positioned): Nothing = {
    report(errorCode, msg, ErrorLevel.Fatal, pos)
    throw new Exception // Won't happen but is needed for type Nothing
  }

  def report(errorCode: Int, msg: String, errorLevel: ErrorLevel, pos: Positioned): Unit = {
    val code = ErrorLetters + errorLevel.num + leftPadCode(errorCode)
    val err = Error(code, msg, errorLevel, pos)
    ctx.reporter.report(err)
  }

  private def leftPadCode(num: Int): String = num match {
    case x if x >= 0 && x < 10     => "00" + x
    case x if x >= 10 && x < 100   => "0" + x
    case x if x >= 100 && x < 1000 => "" + x
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
      sb.toString
    }
  }

}

object Errors {
  val ErrorName = "$ERROR$"
}

case class Error(code: String, msg: String, errorLevel: ErrorLevel, pos: Positioned) {

  private val messageHash = msg.hashCode

  override def equals(obj: Any): Boolean = obj match {
    case e@Error(code0, _, errorLevel0, pos0) =>
      messageHash == e.messageHash && code == code0 && errorLevel == errorLevel0 && pos.equalPos(pos0)
    case _                                    => false
  }

  override val hashCode: Int = messageHash ^ (31 * code.hashCode) ^ (31 * errorLevel.num) ^ (31 * pos.encodedStartPos) ^ (31 * pos.encodedEndPos)
}

trait ErrorLevel {
  def num: Int
}

object ErrorLevel {
  case object Warning extends ErrorLevel {val num = 1}
  case object Error extends ErrorLevel {val num = 2}
  case object Fatal extends ErrorLevel {val num = 3}
}

