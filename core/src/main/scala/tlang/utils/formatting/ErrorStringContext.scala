package tlang.utils.formatting

import tlang.Context
import tlang.compiler.ast.Trees.CompilationUnit
import tlang.compiler.error.{NameSuggestor, Suggestion, TemplateNameParser}


object ErrorStringContext {

  def apply(context: Context, cu: CompilationUnit): ErrorStringContext = apply(
    context.formatting,
    NameSuggestor(),
    cu.imports.replaceNames
  )
}

case class ErrorStringContext(
  formatting: Formatting,
  private val nameSuggestor: NameSuggestor = NameSuggestor(),
  private val replaceNames: String => String = identity[String]
) {

  import formatting._

  def suggestions(name: String, alternatives: List[String]) = nameSuggestor(name, alternatives)

  implicit class ErrorStringContext(val sc: StringContext) {

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
