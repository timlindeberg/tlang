package tlang.formatting.grid

import tlang.utils.Extensions._

import scala.collection.mutable.ListBuffer

object ColumnDefaults {
  val width            = Width.Auto
  val overflowHandling = OverflowHandling.Wrap
  val alignment        = Alignment.Left
}

object Column extends Column(ColumnDefaults.width, ColumnDefaults.alignment, ColumnDefaults.overflowHandling)
object CenteredColumn extends Column(ColumnDefaults.width, Alignment.Center, ColumnDefaults.overflowHandling)
object TruncatedColumn extends Column(ColumnDefaults.width, ColumnDefaults.alignment, OverflowHandling.Truncate)

case class Column(
  width: Width = ColumnDefaults.width,
  alignment: Alignment = ColumnDefaults.alignment,
  overflowHandling: OverflowHandling = ColumnDefaults.overflowHandling) {

  private[grid] val lines: ListBuffer[String] = ListBuffer()
  private       var _maxWidth                 = 0

  def maxWidth: Int = _maxWidth
  def addLine(newLine: String): Unit = {
    _maxWidth = Math.max(_maxWidth, newLine.charCount)
    lines += newLine
  }

  def content: String = lines.mkString("\n")

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
  case class Percentage(width: Double) extends FixedWidth {
    if (width < 0.0 || width > 1.0)
      throw new IllegalArgumentException("Percentage width should be between 0 and 1")

    def apply(maxWidth: Int): Int = (width * maxWidth).toInt
  }
}

trait Alignment {
  def apply(text: String, width: Int, fill: Char = ' '): String = {
    if (width < 1)
      throw new IllegalArgumentException(s"Cannot align text within a space smaller than 1: $width")

    val textWidth = text.charCount
    if (textWidth > width) {
      throw new IllegalArgumentException(s"Cannot align text '$text' in the given space: $textWidth > $width")
    }

    align(text, width - textWidth, fill)
  }

  protected def align(text: String, space: Int, fill: Char): String

}
object Alignment {
  case object Left extends Alignment {
    override def align(text: String, space: Int, fill: Char): String = {
      text + s"$fill" * space
    }
  }
  case object Right extends Alignment {
    override def align(text: String, space: Int, fill: Char): String = {
      s"$fill" * space + text
    }
  }
  case object Center extends Alignment {
    override def align(text: String, space: Int, fill: Char): String = {
      val halfSpace = s"$fill" * (space / 2)
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
