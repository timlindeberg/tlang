package tlang.compiler.error

import tlang.Context
import tlang.compiler.ast.Trees.CompilationUnit
import tlang.formatting.Formatting


object ErrorStringContext {

  def apply(context: Context, cu: CompilationUnit): ErrorStringContext = {
    apply(
      context.formatting,
      AlternativeSuggestor(),
      List[String => String](TemplateNameReplacer.apply, cu.imports.replaceNames)
    )
  }
}

case class ErrorStringContext(
  formatting: Formatting,
  private val alternativeSuggestor: AlternativeSuggestor = AlternativeSuggestor(),
  private val transforms: List[String => String] = Nil
) {

  import formatting._

  def suggestion(name: String, alternatives: List[String]) = alternativeSuggestor(name, alternatives)

  val TextColor  = Bold
  val ValueColor = Bold + NumColor

  implicit class ErrorStringContext(val sc: StringContext) {

    def err(args: Any*): String = {

      val strings = sc.parts.iterator
      val expressions = args.iterator
      val sb = new StringBuilder(TextColor(strings.next))
      while (strings.hasNext) {
        sb ++= evaluate(expressions.next)
        val s = strings.next
        if (s.nonEmpty)
          sb ++= TextColor(s)
      }
      sb.toString
    }

    private def evaluate(any: Any) = any match {
      case Suggestion(suggestion) => err" Did you mean $suggestion?"
      case any                    =>
        val str = Function.chain(transforms)(any.toString)
        if (formatting.useColor) ValueColor(str) else s"'$str'"
    }
  }

}
