package tlang
package formatting

import tlang.formatting.Colors.Color


case class ErrorStringContext(
  private val alternativeSuggestor: AlternativeSuggestor = AlternativeSuggestor(),
  private val transforms: List[String => String] = Nil
)(
  implicit val formatter: Formatter
) {


  def suggestion(name: String, alternatives: List[String]): Suggestion = alternativeSuggestor(name, alternatives)


  implicit class ErrorStringContext(val sc: StringContext) {

    import formatter._


    private val ValueColor: Color = NumColor

    private var currentColor: Color            = NoColor
    private val sb          : StringBuilder    = new StringBuilder
    private val strings     : Iterator[String] = sc.parts.iterator
    private var nextString  : String           = strings.next
    private var expressions : Iterator[Any]    = _

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

      suggestions.distinct match {
        case suggestion :: Nil =>
          val transformed = transform(suggestion)
          val v           = if (formatter.useColor) ValueColor(transformed) else s"'$transformed'"
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
              sb ++= s"   $ListMarker " + ValueColor + transform(suggestion)
              currentColor = ValueColor
              sb.toString
            }
            .mkString(NL)
          if (hasMore)
            sb ++= NL
      }
    }

    private def transform(any: Any): String = Function.chain(transforms)(String.valueOf(any))

    private def evaluateAny(any: Any): Unit = {
      val str = transform(any)
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
