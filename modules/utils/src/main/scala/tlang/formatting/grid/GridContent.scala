package tlang
package formatting
package grid

import tlang.formatting.Colors.Color

trait GridContent {
  def render(width: Int): String
  def width: Option[Int] = None
  def size: Option[Int] = None
}

case class StringContent(any: Any)(implicit val formatter: Formatter) extends GridContent {
  val string: String = formatter.replaceTabs(any.toString)
  def render(width: Int): String = string
  override def width: Option[Int] = {
    val width = if (string.isEmpty) 0 else string.lines.map(_.visibleCharacters).max
    Some(width)
  }
}

case class CenteredContent(content: Any, color: Color = Colors.NoColor, fill: String = " ")
  (implicit val formatter: Formatter) extends GridContent {
  def render(width: Int): String = {
    val s = formatter.replaceTabs(content.toString)
    color(Alignment.Center(s, width, fill))
  }
}

case class Divider(fill: String, color: Color = Colors.NoColor) extends GridContent {
  def render(width: Int): String = {
    val charWidth = fill.visibleCharacters
    val divider = (fill * (width / charWidth)) + fill.take(width % charWidth)
    color(divider)
  }
}

case class EvenlySpaced(items: Iterable[String], spacing: Int = 1)
  (implicit val formatter: Formatter) extends GridContent {

  def render(width: Int): String = {
    val strings = items.map { formatter.replaceTabs(_) }
    val columnWidth = strings.map(_.visibleCharacters).max + spacing
    val numColumns = (width + spacing) / columnWidth

    if (numColumns == 0)
      return strings.map(pad(_, width)).mkString(NL)

    val totalWidth = numColumns * columnWidth - spacing

    strings
      .grouped(numColumns)
      .map { columns =>
        val x = columns
          .zipWithIndex
          .map { case (s, i) =>
            var padding = columnWidth - s.visibleCharacters - spacing
            if (i != columns.size - 1) padding += spacing
            s + " " * padding
          }
          .mkString("")

        pad(x, totalWidth)
      }
      .mkString(NL)
  }

  private def pad(s: String, width: Int) = s.padTo(width + s.length - s.visibleCharacters, ' ')
}
