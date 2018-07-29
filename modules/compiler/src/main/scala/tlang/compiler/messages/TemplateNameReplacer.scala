package tlang
package compiler
package messages

import scala.util.parsing.combinator.RegexParsers

// Can only replace one template string per string which is fine for it's use case
object TemplateNameReplacer extends RegexParsers {

  import tlang.compiler.modification.Templating._

  def apply(s: String): String = {
    val first = s.indexOf('-')
    if (first == -1)
      return s

    val last = s.lastIndexOf('-')
    if (last == -1)
      return s

    val preFix = s.substring(0, first)
    val middle = s.substring(first, last + 1)
    val postFix = s.substring(last + 1, s.length)

    parseAll(template, middle) match {
      case Success(res, _) => preFix + res + postFix
      case NoSuccess(_, _) => s
      case Failure(_, _)   => s
    }
  }

  // Grammar

  private def word: Parser[String] =
    """[a-zA-Z\d_]+""".r ^^ {
      _.toString
    }

  private def typeList: Parser[String] = ((Seperator ~ (word | template)) +) ^^ (list => list.map(_._2).mkString(", "))

  private def template: Parser[String] = StartEnd ~ word ~ typeList ~ StartEnd ^^ {
    case _ ~ word ~ args ~ _ => s"$word<$args>"
  }
}
