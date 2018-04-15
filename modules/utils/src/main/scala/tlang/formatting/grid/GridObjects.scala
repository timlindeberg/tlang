package tlang.formatting.grid

import tlang.formatting.Colors
import tlang.formatting.Colors.Color
import tlang.utils.Extensions._

import scala.collection.mutable.ListBuffer

trait GridContent {

  def render(width: Int): String

  def width: Option[Int] = None
}

case class StringContent(any: Any) extends GridContent {
  private val s = any.toString
  def render(width: Int): String = s
  override def width: Option[Int] = {
    val width = if (s.isEmpty) 0 else s.lines.map(_.visibleCharacters).max
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

object ColumnDefaults {
  val Width            = tlang.formatting.grid.Width.Auto
  val OverflowHandling = tlang.formatting.grid.OverflowHandling.Wrap
  val Alignment        = tlang.formatting.grid.Alignment.Left
}

object Column extends Column(ColumnDefaults.Width, ColumnDefaults.Alignment, ColumnDefaults.OverflowHandling)
object CenteredColumn extends Column(ColumnDefaults.Width, Alignment.Center, ColumnDefaults.OverflowHandling)
object TruncatedColumn extends Column(ColumnDefaults.Width, ColumnDefaults.Alignment, OverflowHandling.Truncate)

case class Column(
  width: Width = ColumnDefaults.Width,
  alignment: Alignment = ColumnDefaults.Alignment,
  overflowHandling: OverflowHandling = ColumnDefaults.OverflowHandling) {

  private[grid] val lines: ListBuffer[GridContent] = ListBuffer()
  private       var _maxWidth                      = 0

  def maxWidth: Int = _maxWidth
  def addLine(newLine: String): Unit = addLine(StringContent(newLine))

  def addLine(content: GridContent): Unit = {
    content.width ifDefined { width => _maxWidth = Math.max(_maxWidth, width) }
    lines += content
  }

  def content: String = lines.map(content => content.render(_maxWidth)).mkString("\n")


}

trait Width
trait FixedWidth extends Width {
  def apply(maxWidth: Int): Int
}

object Width {
  case object Auto extends Width
  case class Fixed(width: Int) extends FixedWidth {
    def apply(maxWidth: Int): Int = width
  }
  case class Percentage(widthPercentage: Double) extends FixedWidth {
    if (widthPercentage < 0.0 || widthPercentage > 1.0)
      throw new IllegalArgumentException("Percentage width should be between 0 and 1")

    def apply(maxWidth: Int): Int = (widthPercentage * maxWidth).toInt
  }
}

trait Alignment {
  def apply(text: String, width: Int, fill: String = " "): String = {
    if (width < 1)
      throw new IllegalArgumentException(s"Cannot align text within a space smaller than 1: $width")

    val textWidth = text.visibleCharacters
    if (textWidth > width)
      throw new IllegalArgumentException(s"Cannot align text '$text' in the given space: $textWidth > $width")

    align(text, width - textWidth, fill)
  }

  protected def align(text: String, space: Int, fill: String): String

}

object Alignment {
  case object Left extends Alignment {
    override def align(text: String, space: Int, fill: String): String = {
      text + fill * space
    }
  }
  case object Right extends Alignment {
    override def align(text: String, space: Int, fill: String): String = {
      fill * space + text
    }
  }
  case object Center extends Alignment {
    override def align(text: String, space: Int, fill: String): String = {
      val halfSpace = fill * (space / 2)
      val left = halfSpace
      val right = if (space % 2 == 0) halfSpace else halfSpace + fill
      left + text + right
    }
  }
}

trait OverflowHandling
object OverflowHandling {

  case object Except extends OverflowHandling
  case object Wrap extends OverflowHandling
  case object Truncate extends OverflowHandling
}