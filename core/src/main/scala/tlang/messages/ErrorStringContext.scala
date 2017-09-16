package tlang.messages

import tlang.Context
import tlang.compiler.ast.Trees.CompilationUnit
import tlang.formatting.Colors.Color
import tlang.formatting.{Formatter, SimpleFormatting}
import tlang.utils.Extensions._


object ErrorStringContext {

  def apply(): ErrorStringContext = ErrorStringContext(Formatter(SimpleFormatting))

  def apply(context: Context, cu: CompilationUnit): ErrorStringContext = {
    apply(
      context.formatter,
      AlternativeSuggestor(),
      List[String => String](TemplateNameReplacer.apply, cu.imports.replaceNames)
    )
  }
}

case class ErrorStringContext(
  formatter: Formatter,
  private val alternativeSuggestor: AlternativeSuggestor = AlternativeSuggestor(),
  private val transforms: List[String => String] = Nil
) {


  def suggestion(name: String, alternatives: List[String]) = alternativeSuggestor(name, alternatives)


  implicit class ErrorStringContext(val sc: StringContext) {

    import formatter.formatting._


    val ValueColor = NumColor

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
          val v = if (formatter.useColor) ValueColor(suggestion) else s"'$suggestion'"
          sb ++= " Did you mean " + v + Bold + "?"
          currentColor = Bold
          if (hasMore)
            sb ++= " "
        case suggestions       =>
          sb ++= " Did you mean?" + NL
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
            .mkString(NL)
          if (hasMore)
            sb ++= NL
      }
    }


    private def evaluateAny(any: Any): Unit = {
      val str = Function.chain(transforms)(any.toString)
      if (!formatter.useColor) {
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
