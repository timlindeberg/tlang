package tlang.formatting.grid

import tlang.formatting.Colors
import tlang.formatting.Colors.Color
import tlang.utils.Extensions._

trait GridContent {
  def render(width: Int): String
  def width: Option[Int] = None
  def size: Option[Int] = None
}

case class StringContent(any: Any) extends GridContent {
  val string: String = any.toString
  def render(width: Int): String = string
  override def width: Option[Int] = {
    val width = if (string.isEmpty) 0 else string.lines.map(_.visibleCharacters).max
    Some(width)
  }
}

case class CenteredContent(content: Any, color: Color = Colors.NoColor, fill: String = " ") extends GridContent {
  def render(width: Int): String = color(Alignment.Center(content.toString, width, fill))
}

case class Divider(fill: String, color: Color = Colors.NoColor) extends GridContent {
  def render(width: Int): String = {
    val charWidth = fill.visibleCharacters
    val divider = (fill * (width / charWidth)) + fill.take(width % charWidth)
    color(divider)
  }
}

case class EvenlySpaced(items: Iterable[String], spacing: Int = 1) extends GridContent {

  def render(width: Int): String = {
    val columnWidth = items.map(_.visibleCharacters).max + spacing
    val numColumns = (width + spacing) / columnWidth

    if (numColumns == 0)
      return items.map(pad(_, width)).mkString(NL)

    val totalWidth = numColumns * columnWidth - spacing

    items
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