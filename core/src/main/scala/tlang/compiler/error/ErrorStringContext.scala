package tlang.compiler.error

import tlang.Context
import tlang.compiler.ast.Trees.CompilationUnit
import tlang.formatting.Colors.Color
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

  val ValueColor = NumColor

  implicit class ErrorStringContext(val sc: StringContext) {

    var currentColor: Color            = NoColor
    val sb          : StringBuilder    = new StringBuilder
    val strings     : Iterator[String] = sc.parts.iterator
    var nextString  : String           = strings.next
    var expressions : Iterator[Any]    = _

    def err(args: Any*): String = {

      expressions = args.iterator

      if (nextString.isEmpty && !expressions.hasNext)
        return ""

      if (nextString.nonEmpty) {
        sb ++= Bold + nextString
        currentColor = Bold
      }

      while (strings.hasNext) {
        nextString = strings.next

        expressions.next match {
          case Suggestion(suggestions) => evaluateSuggestion(suggestions)
          case any                     => evaluateAny(any)

        }
        if (nextString.nonEmpty) {
          if (currentColor != Bold) {
            if (currentColor == ValueColor) {
              sb ++= Reset
            }
            sb ++= Bold
            currentColor = Bold
          }
          sb ++= nextString
        }
      }
      if (currentColor != NoColor)
        sb ++= Reset
      sb.toString
    }

    private def evaluateSuggestion(suggestions: List[String]): Unit = {
      val hasMore = nextString.nonEmpty || expressions.hasNext
      if (suggestions.isEmpty) {
        if (hasMore)
          sb ++= " "
        return
      }

      if (currentColor != Bold) {
        if (currentColor == ValueColor)
          sb ++= Reset
        sb ++= Bold
        currentColor = Bold
      }

      suggestions match {
        case suggestion :: Nil =>
          val v = if (formatting.useColor) ValueColor(suggestion) else s"'$suggestion'"
          sb ++= " Did you mean " + v + Bold + "?"
          currentColor = Bold
          if (hasMore)
            sb ++= " "
        case suggestions       =>
          sb ++= " Did you mean?" + System.lineSeparator
          sb ++= suggestions
            .map { suggestion =>
              val sb = new StringBuilder
              if (currentColor != Bold) {
                sb ++= Reset
                sb ++= Bold
              }
              sb ++= s"   $ListMarker " + ValueColor + suggestion
              currentColor = ValueColor
              sb.toString
            }
            .mkString(System.lineSeparator)
          if (hasMore)
            sb ++= System.lineSeparator
      }
    }


    private def evaluateAny(any: Any): Unit = {
      val str = Function.chain(transforms)(any.toString)
      if (!formatting.useColor) {
        sb ++= s"'$str'"
        return
      }
      if (currentColor != ValueColor) {
        sb ++= (if (currentColor == Bold) ValueColor else Bold + ValueColor)
        currentColor = ValueColor
      }
      sb ++= str
    }
  }

}
