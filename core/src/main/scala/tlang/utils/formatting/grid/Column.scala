package tlang.utils.formatting.grid

import tlang.utils.Extensions._
import tlang.utils.formatting.AnsiWordWrapper

import scala.collection.mutable.ListBuffer

object ColumnDefaults {
  val width            = Width.Auto
  val overflowHandling = OverflowHandling.Wrap
  val alignment        = Alignment.Left
}

object Column extends Column(ColumnDefaults.width, ColumnDefaults.alignment, ColumnDefaults.overflowHandling)
case class Column(
  width: Width = ColumnDefaults.width,
  alignment: Alignment = ColumnDefaults.alignment,
  overflowHandling: OverflowHandling = ColumnDefaults.overflowHandling) {

  private[grid] val lines: ListBuffer[String] = ListBuffer()
  private       var _maxWidth                 = 0

  def maxWidth: Int = _maxWidth
  def addLines(newLines: Seq[String]): Unit = {
    val newLinesMaxWidth = newLines.map(_.charCount).max
    _maxWidth = Math.max(_maxWidth, newLinesMaxWidth)
    lines ++= newLines
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
    if (textWidth > width)
      throw new IllegalArgumentException(s"Cannot align text $text in the given space: $textWidth > $width")

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

trait OverflowHandling {
  def apply(line: String, width: Int): List[String] = {
    if (width < 1)
      throw new IllegalArgumentException(s"Cannot handle overflow of text within a space smaller than 1: $width")
    handle(line, width)
  }

  def handle(line: String, width: Int): List[String]

}
object OverflowHandling {

  case object Except extends OverflowHandling {
    def handle(line: String, width: Int): List[String] = {
      val lineWidth = line.charCount
      if (lineWidth > width)
        throw new IllegalStateException(s"Cannot fit line $line in the given space: $lineWidth > $width")
      List(line)
    }
  }

  case object Wrap extends OverflowHandling {
    private val wordWrapper = AnsiWordWrapper()
    def handle(line: String, width: Int): List[String] = wordWrapper(line, width)
  }

  case object Truncate extends OverflowHandling {

    private val TruncationChar : Char   = '.'
    private val TruncationWidth: Int    = 3
    private val Truncation     : String = s"$TruncationChar" * TruncationWidth

    def handle(line: String, width: Int): List[String] = List(truncate(line, width))

    private def truncate(line: String, width: Int): String = {
      val lineWidth = line.charCount
      if (lineWidth <= width)
        return line

      if (width <= TruncationWidth)
        return s"$TruncationChar" * width

      var ansiChars = 0
      var lastAnsi = ""
      var i = 0
      while (i < line.length) {
        val c = line(i)
        c match {
          case '\u001b' =>
            val endOfAnsi = line.indexOf('m', i + 1)
            ansiChars += endOfAnsi - i + 1
            lastAnsi = line.substring(i, endOfAnsi + 1)
            i = endOfAnsi
          case _        =>
            val realChars = i - ansiChars
            if (realChars == width - TruncationWidth) {
              val truncated = line.substring(0, i)
              if (lastAnsi.isEmpty || lastAnsi == Console.RESET)
                return truncated + Truncation
              return truncated + Console.RESET + Truncation
            }
        }
        i += 1
      }
      line
    }
  }
}
