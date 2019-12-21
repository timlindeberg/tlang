package tlang
package compiler
package messages

import scala.util.parsing.combinator.RegexParsers

object TemplateNameReplacer extends RegexParsers {

  import tlang.compiler.modification.Templating._

  def apply(content: String): String = {
    parseAll(full, content) match {
      case Success(res, _) => res
      case NoSuccess(_, _) => content
      case Failure(_, _)   => content
    }
  }

  // Grammar

  private def full: Parser[String] = textAndTemplate.* ^^ { values => values.mkString }

  private def textAndTemplate: Parser[String] = (anyText.? ~ template ~ anyText.?) ^^ { case before ~ replacedTemplate ~ after =>
    before.getOrElse("") + replacedTemplate + after.getOrElse("")
  }

  private def anyText: Parser[String] = """([^\n\-]+)""".r ^^ { _.toString }

  private def word: Parser[String] = """[a-zA-Z\d_]+""".r ^^ { _.toString }

  private def typeList: Parser[String] = ((Seperator ~ (word | template)).+) ^^ { list =>
    list.map(_._2).mkString(", ")
  }

  private def template: Parser[String] = StartEnd ~ word ~ typeList ~ StartEnd ^^ {
    case _ ~ word ~ args ~ _ => s"$word<$args>"
  }
}
