package tlang.utils.formatting

import java.io.File

import tlang.Constants
import tlang.compiler.ast.{PrettyPrinter, TreePrinter}
import tlang.compiler.options.Flags.LineWidth
import tlang.utils.Extensions._
import tlang.utils.formatting.BoxStyles.BoxStyle
import tlang.utils.formatting.Colors.{Color, ColorScheme, DefaultColorScheme}
import tlang.utils.formatting.ColumnWidth.{Auto, Fixed, Percentage}

import scala.collection.mutable.ListBuffer

object FancyFormatting extends Formatting(
  BoxStyles.Light, LineWidth.DefaultWidth, useColor = true, asciiOnly = false
)
object SimpleFormatting extends Formatting(
  BoxStyles.Simple, LineWidth.DefaultWidth, useColor = false, asciiOnly = true
)

trait ColumnWidth
object ColumnWidth {
  case object Auto extends ColumnWidth
  case class Fixed(width: Int) extends ColumnWidth
  case class Percentage(width: Double) extends ColumnWidth {
    if (width < 0.0 || width > 1.0)
      throw new IllegalArgumentException("ColumnWidth.Percentage width should be between 0 and 1")
  }
}

trait Alignment {
  def apply(text: String, width: Int, fill: Char = ' '): String = {
    val textWidth = text.charCount
    if (textWidth > width)
      throw new IllegalArgumentException(s"Cannot align text $text in the given space: $textWidth > $width ")

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
  def apply(line: String, width: Int): List[String]
}
object OverflowHandling {

  case object Except extends OverflowHandling {
    def apply(line: String, width: Int): List[String] = {
      if (line.charCount > width)
        throw new IllegalStateException(s"Cannot fit line $line in the given space: ${ line.length } > $width")
      List(line)
    }
  }

  case object Wrap extends OverflowHandling {
    private val wordWrapper = AnsiWordWrapper()
    def apply(line: String, width: Int): List[String] = wordWrapper(line, width)
  }

  case object Truncate extends OverflowHandling {
    def apply(line: String, width: Int): List[String] = List(truncate(line, width))

    private def truncate(line: String, width: Int): String = {
      val lineWidth = line.charCount
      if (lineWidth <= width)
        return line

      if (width <= 3)
        return "." * width

      var ansiChars = 0
      var i = 0
      var lastAnsi = ""
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
            if (realChars == width - 3) {
              val truncated = line.substring(0, i)
              if (lastAnsi.isEmpty || lastAnsi == Console.RESET)
                return truncated + "..."
              return truncated + Console.RESET + "..."
            }
        }
        i += 1
      }
      line
    }
  }
}

case class Column(
  widthType: ColumnWidth = ColumnWidth.Auto,
  handleOverflow: OverflowHandling = OverflowHandling.Wrap,
  alignment: Alignment = Alignment.Left) {

  private[formatting] val lines: ListBuffer[String] = ListBuffer()

  var maxWidth: Int = 0
  def addLines(newLines: Seq[String]): Unit = {
    val newLinesMaxWidth = newLines.map(_.charCount).max
    maxWidth = Math.max(maxWidth, newLinesMaxWidth)
    lines ++= newLines
  }

  def content: String = lines.mkString("\n")

}

case class Grid(var formatting: Formatting) {

  private val rows: ListBuffer[Row] = ListBuffer()
  private var indent                = 1

  private var _currentRow: Option[Row] = None
  private def currentRow: Row = {
    if (_currentRow.isEmpty) {
      row()
    }
    _currentRow.get
  }

  def apply(i: Int): Row = rows(i)
  def size: Int = rows.size

  def formatting(formatting: Formatting): Grid = {
    this.formatting = formatting
    this
  }

  def indent(indent: Int): Grid = {
    this.indent = indent
    this
  }

  def row(): Grid = row(Column())
  def row(textAlignment: Alignment): Grid = row(Column(alignment = textAlignment))
  def row(column: Column, moreColumns: Column*): Grid = {
    val row = Row(rows.length, column :: moreColumns.toList)
    _currentRow = Some(row)
    rows += row
    this
  }

  def content(content: String, moreContent: String*): Grid = {
    val allContent = content :: moreContent.toList
    verifyNumValues(allContent.length)
    addContent(allContent)
    this
  }

  def content(content: Seq[String], moreContent: Seq[String]*): Grid = {
    val allContent = content :: moreContent.toList
    verifyNumValues(allContent.length)
    allContent.transpose foreach addContent
    this
  }

  def columnWidths: Seq[Int] = currentRow.columnWidths

  private def addContent(allContent: Seq[String]) = {
    currentRow.columns.zip(allContent).foreach { case (column, content) =>
      val lines = content.split('\n').toList
      column.addLines(lines)
    }
  }

  private def verifyNumValues(contentLength: Int) = {
    val columnLength = currentRow.columns.length
    if (contentLength != columnLength) {
      throw new IllegalArgumentException(
        s"Number of column values given to content doesn't match the number of columns in the row: $contentLength != $columnLength"
      )
    }
  }

  override def toString: String = {
    val sb = new StringBuilder

    var i = 0

    sb ++= drawTop(rows.head)
    sb ++= "\n"
    while (i < rows.size) {
      val row = rows(i)
      drawContent(sb, row)

      if (i < rows.size - 1) {
        sb ++= drawMiddle(row, rows(i + 1))
        sb ++= "\n"
      }
      i += 1
    }
    sb ++= drawBottom(rows.last)
    sb.toString
  }

  private def drawTop(row: Row) = {
    val box = formatting.box
    import box._

    _drawTopOrBottom(┌, ┬, ┐, row)
  }

  private def drawBottom(row: Row): String = {
    val box = formatting.box
    import box._

    _drawTopOrBottom(└, ┴, ┘, row)
  }

  private def _drawTopOrBottom(left: String, middle: String, right: String, row: Row): String = {
    val box = formatting.box
    import box._
    val sb = new StringBuilder
    sb ++= left
    row.columnWidths.zipWithIndex.foreach { case (width, index) =>
      val num = 2 * indent + width
      sb ++= ─ * num
      if (index != row.columns.size - 1) {
        sb ++= middle
      }
    }

    sb ++= right
    trimRight(sb.toString)
  }

  // Returns a list of positions where the row has a column break
  private def getLinePositions(row: Row): Iterator[Int] = {
    val widths = row.columnWidths
    var acc = 2 * indent + widths.head
    widths.tail
      .map { width =>
        val p = acc
        acc += width + 2 * indent + 1
        p
      }
      .iterator
  }

  private def drawMiddle(before: Row, after: Row) = {
    val box = formatting.box
    import box._
    val sb = new StringBuilder
    val maxWidth = formatting.lineWidth
    sb ++= ├
    val upPositions = getLinePositions(before)
    val downPositions = getLinePositions(after)
    var up = upPositions.next()
    var down = downPositions.next()
    var x = 0
    while (x < maxWidth - 2) {
      val X = x // X is a stable identifier, we cant use x since it's a var
      sb ++= ((up, down) match {
        case (X, X) => ┼
        case (X, _) => ┴
        case (_, X) => ┬
        case _      => ─
      })

      if (x >= up && upPositions.hasNext)
        up = upPositions.next()

      if (x >= down && downPositions.hasNext)
        down = downPositions.next()
      x += 1
    }
    sb ++= ┤
    trimRight(sb.toString)
  }

  private def drawContent(sb: StringBuilder, row: Row): Unit = {
    // Col(Lines(1, 2, 3, 4),
    val box = formatting.box
    import box._

    val columns = row.columns
    val columnWidths = row.columnWidths
    sb ++= row.columns
      .map(_.lines) // Take each set of lines for reach column
      .transpose // Transpose to get the corresponding line each column together
      .map { lines =>
      // Word wrap each line in each column
      val wrappedLines = lines.zipWithIndex.map { case (line, columnIndex) =>
        columns(columnIndex).handleOverflow(line, columnWidths(columnIndex))
      }
      val maxNumLines = wrappedLines.map(_.length).max
      // Fill out the columns with empty lines so that each column has the same
      // number of lines after word wrapping
      wrappedLines.map { lines => lines ::: List.fill(maxNumLines - lines.size)("") }
    }
      .transpose // Transpose back so that we have a list of list of lines for each column
      .map(_.flatten) // Flatten the list so we just have one list of lines for each column
      .transpose
      .map { lines =>
        // Draw each line
        val content = lines.zipWithIndex.map { case (line, columnIndex) =>
          columns(columnIndex).alignment(line, columnWidths(columnIndex))
        }
        val fill = " " * indent
        trimRight(│ + fill + content.mkString(fill + │ + fill) + fill + │)
      }
      .mkString("\n")
    sb ++= "\n"
  }

  private def trimRight(s: String) = s.rightTrimWhiteSpaces

  case class Row(rowIndex: Int, columns: Seq[Column] = Seq(Column())) {

    def apply(i: Int): Column = columns(i)
    def size: Int = columns.size
    def height: Int = 0

    def columnWidths: Seq[Int] = {
      val maxWidth = formatting.lineWidth
      val indentAndBorderSize = 2 + (2 * indent) + ((columns.length - 1) * (1 + 2 * indent))
      val actualSpace = maxWidth - indentAndBorderSize

      val fixedWidth = columns
        .map(_.widthType)
        .collect {
          case Fixed(w)      => w
          case Percentage(w) => (w * actualSpace).toInt
        }
        .sum

      val numAuto = columns.count(_.widthType == ColumnWidth.Auto)

      val neededWidth = fixedWidth + 1 * numAuto
      if (neededWidth > actualSpace) {
        throw new IllegalStateException(s"The width of columns in row $rowIndex is larger than the total width. $neededWidth > $actualSpace.")
      }

      if (numAuto == 0) {
        // If all columns have fixed width we let the last one take up the remaining space
        columnWidthsOnlyFixedSize(actualSpace)
      } else {
        // Otherwise the last auto should take up the remaining space
        columnWidthsWithAuto(actualSpace, fixedWidth, numAuto)
      }
    }

    private def columnWidthsOnlyFixedSize(actualSpace: Int) = {
      var remainingWidth = actualSpace
      columns.zipWithIndex.map { case (column, i) =>
        val width = if (i == columns.length - 1)
          remainingWidth
        else column.widthType match {
          case Fixed(w)      => w
          case Percentage(w) => (w * actualSpace).toInt
        }

        remainingWidth -= width
        width
      }
    }

    private def columnWidthsWithAuto(actualSpace: Int, fixedWidth: Int, numAuto: Int) = {
      var remainingWidth = actualSpace - fixedWidth
      var remainingAutos = numAuto
      val widths = Array.ofDim[Int](columns.length)

      // We sort the columns by maximum width. We want to select the smallest
      // ones first in order to maximize the available space
      columns.zipWithIndex.sortBy(_._1.maxWidth).foreach { case (column, index) =>
        val width = column.widthType match {
          case Auto          =>
            if (remainingAutos == 1) {
              remainingWidth
            } else {
              val possibleSpace = remainingWidth.toDouble / remainingAutos
              remainingAutos -= 1

              // If rounding up makes the content fit, we do that but otherwise
              // we round down. If the maxWidth is less than the available space
              // we only use the maximum width
              val w = if (Math.ceil(possibleSpace) == column.maxWidth || column.maxWidth <= possibleSpace) {
                column.maxWidth
              } else {
                possibleSpace.toInt
              }
              remainingWidth -= w
              w
            }
          case Fixed(w)      => w
          case Percentage(w) => (w * actualSpace).toInt
        }
        widths(index) = width
      }
      widths.toList
    }

  }

}


case class Formatting(
  box: BoxStyle = BoxStyles.Light,
  lineWidth: Int = LineWidth.DefaultWidth,
  colorScheme: ColorScheme = DefaultColorScheme,
  useColor: Boolean = true,
  asciiOnly: Boolean = false,
  trim: Boolean = true) {

  import box._

  import Console._

  /*--------------------------------- Colors --------------------------------*/

  val NoColor   = Color("", isActive = false)
  val Reset     = Color(RESET, useColor)
  val Bold      = Color(BOLD, useColor)
  val Underline = Color(UNDERLINED, useColor)

  val Black   = Color(BLACK, useColor)
  val Red     = Color(RED, useColor)
  val Green   = Color(GREEN, useColor)
  val Yellow  = Color(YELLOW, useColor)
  val Blue    = Color(BLUE, useColor)
  val Magenta = Color(MAGENTA, useColor)
  val Cyan    = Color(CYAN, useColor)
  val White   = Color(WHITE, useColor)

  val BlackBG   = Color(BLACK_B, useColor)
  val RedBG     = Color(RED_B, useColor)
  val GreenBG   = Color(GREEN_B, useColor)
  val YellowBG  = Color(YELLOW_B, useColor)
  val BlueBG    = Color(BLUE_B, useColor)
  val MagentaBG = Color(MAGENTA_B, useColor)
  val CyanBG    = Color(CYAN_B, useColor)
  val WhiteBG   = Color(WHITE_B, useColor)

  val AllColors: Array[Color] = Array(Black, Red, Green, White, Yellow, Blue, Magenta, Cyan, White)

  /*------------------------------ Color Scheme -----------------------------*/

  val KeywordColor     = Color(colorScheme.Keyword, useColor)
  val VarColor         = Color(colorScheme.Variable, useColor)
  val ClassColor       = Color(colorScheme.Class, useColor)
  val MethodColor      = Color(colorScheme.Method, useColor)
  val StringColor      = Color(colorScheme.String, useColor)
  val NumColor         = Color(colorScheme.Number, useColor)
  val CommentColor     = Color(colorScheme.Comment, useColor)
  val SymbolColor      = Color(colorScheme.Symbol, useColor)
  val FileColor: Color = Bold + Magenta

  /*------------------------------- Utilities -------------------------------*/

  val wordWrapper           = AnsiWordWrapper()
  val syntaxHighlighter     = SyntaxHighlighter(this)
  val stackTraceHighlighter = StackTraceHighlighter(this)
  val prettyPrinter         = PrettyPrinter(this)
  val treePrinter           = TreePrinter(this)

  /*----------------------------- ASCII Variants ----------------------------*/

  def ListMarker: String = ascii("*", "•")
  def Cross: String = ascii("x", "×")

  def spinner: Spinner = ascii(ASCIISpinner(), BrailSpinner())

  private def ascii[T](ascii: T, nonAscii: T): T = if (asciiOnly) ascii else nonAscii

  /*------------------------------ Box handling -----------------------------*/

  private val Indent     = 2
  private val Width: Int = lineWidth - (2 * Indent)

  def top: String = trimRight(┌ + ─ * (lineWidth - Indent) + ┐) + "\n"
  def bottom: String = trimRight(└ + ─ * (lineWidth - Indent) + ┘) + "\n"
  def divider: String = trimRight(├ + ─ * (lineWidth - Indent) + ┤) + "\n"

  def seperator(left: String, bridge: String, right: String, bridgeAt: Int): String = {
    val rest = ─ * (lineWidth - bridgeAt - (2 * Indent + 1))
    val overNumbers = ─ * (bridgeAt + Indent)
    val line = left + overNumbers + bridge + rest + right
    trimRight(line) + "\n"
  }

  def rightAlign(text: String, fill: Char = ' '): String =
    wordWrapper(text, Width).map { line =>
      ("" + fill) * (Width - line.charCount) + line
    }.mkString("\n")

  def center(text: String, fill: Char = ' '): String =
    wordWrapper(text, Width).map { line =>
      val x = Width - line.charCount
      val space = ("" + fill) * (x / 2)
      val left = space
      val right = if (x % 2 == 0) space else space + fill
      left + line + right
    }.mkString("\n")

  def makeHeader(text: String): String = {
    val lines = makeLines(center(text)).mkString
    top + lines
  }

  def makeLines(lines: String, w: Int = Width): String =
    wordWrapper(lines, w).map(makeLine(_, w)).mkString

  def makeLine(line: String, w: Int = Width): String = {
    val whitespaces = " " * (w - line.charCount)
    val l = │ + " " + line + whitespaces + " " + │
    trimRight(l) + "\n"
  }

  def makeBlock(block: String): String = {
    val sb = new StringBuilder
    sb ++= divider
    sb ++= makeLines(block)
    sb.toString()
  }

  def makeBlockWithColumn(block: Traversable[(String, String)], endOfBlock: Boolean): String = {
    val sb = new StringBuilder

    val columnVars = block.unzip._1
    val maxColumnWidth = columnVars.map(_.charCount).max
    val w = Width - maxColumnWidth - 3

    sb ++= seperator(├, ┬, ┤, maxColumnWidth)

    sb ++= block
      .flatMap { case (col, line) =>
        val lines = wordWrapper(line, w)
        val columns = col :: List.fill(lines.size - 1)("")
        columns zip lines
      }
      .map { case (col, line) =>
        val columnWidth = col.charCount
        val whiteSpaces = " " * (maxColumnWidth - columnWidth)
        │ + " " + col + whiteSpaces + " " + makeLine(line, w)
      }.mkString

    sb ++= (if (endOfBlock) seperator(└, ┴, ┘, maxColumnWidth) else seperator(├, ┴, ┤, maxColumnWidth))
    sb.toString
  }

  def makeBoxWithColumn(header: String, block: Traversable[(String, String)], endOfBlock: Boolean = true): String = {
    val sb = new StringBuilder
    sb ++= makeHeader(header)
    sb ++= makeBlockWithColumn(block, endOfBlock)
    sb.toString
  }

  def makeBox(header: String, blocks: Traversable[String]): String = {
    val sb = new StringBuilder
    sb ++= makeHeader(header)
    blocks foreach { sb ++= makeBlock(_) }
    sb ++= bottom
    sb.toString
  }

  def formatFileName(file: Option[File]): String = {
    file match {
      case Some(f) => formatFileName(f)
      case None    => FileColor("No file")
    }
  }

  def formatFileName(file: File): String = formatFileName(file.getName.replaceAll("\\..*$", ""))

  def formatFileName(name: String): String = FileColor(name + Constants.FileEnding)

  def makeList(items: Traversable[String], indent: String = "  "): String = {
    items.map(item => s"$indent$ListMarker $item").mkString("\n")
  }

  private def trimRight(s: String) = if (trim) s.rightTrimWhiteSpaces else s

}
