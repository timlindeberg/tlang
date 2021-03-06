package tlang
package formatting
package grid

import tlang.formatting.Colors.Color
import tlang.utils.{Memoize, Memoized}

import scala.collection.mutable.ArrayBuffer

case class Grid()(implicit var formatter: Formatter) {

  private val rows: ArrayBuffer[Row] = ArrayBuffer()
  private var indent: Int = 1
  private var borderColor: Color = formatter.NoColor
  private var columnHeaderColor: Color = formatter.Blue + formatter.Bold
  private var shouldTrim: Boolean = true
  private var drawTop: Boolean = true
  private var drawBottom: Boolean = true

  private var _currentRow: Option[Row] = None
  private def currentRow: Row = {
    if (_currentRow.isEmpty)
      row()
    _currentRow.get
  }

  def apply(i: Int): Row = rows(i)
  def size: Int = rows.size
  def clear(): Unit = {
    rows.clear()
    _currentRow = None
  }

  def formatter(formatter: Formatter): Grid = {
    this.formatter = formatter
    this
  }

  def indent(indent: Int): Grid = {
    this.indent = indent
    this
  }

  def columnHeaderColor(color: Color): Grid = {
    this.columnHeaderColor = color
    this
  }

  def borderColor(color: Color): Grid = {
    this.borderColor = color
    this
  }

  def trim(shouldTrim: Boolean): Grid = {
    this.shouldTrim = shouldTrim
    this
  }

  def header(content: String): Grid = {
    if (_currentRow.isDefined)
      throw new IllegalStateException("Cannot add a header once a row has been added.")

    addRow(List(Column(alignment = Alignment.Center)), isHeader = true)
    addContent(List(content))
  }

  def removeTop(): Grid = {
    this.drawTop = false
    this
  }

  def removeBottom(): Grid = {
    this.drawBottom = false
    this
  }

  def row(
    width: Width = Width.Auto,
    handleOverflow: OverflowHandling = OverflowHandling.Wrap,
    alignment: Alignment = Alignment.Left
  ): Grid =
    row(Column(width, alignment, handleOverflow))

  def row(numColumns: Int): Grid = row(List.fill(numColumns)(Column.copy()))

  def row(column: Column, moreColumns: Column*): Grid = {
    // We copy the columns. This allows us to pass a Column object to receive a column with default values.
    // That way we can write .row(Column, Column) instead of .row(Column(), Column())
    // It also allows us to pass the same column multiple times.
    // You also should not be able to pass a column with preexisting state.
    val columns = (column :: moreColumns.toList) map { _.copy() use { _.lines.clear() } }
    row(columns)
  }

  def row(columns: Iterable[Column]): Grid = addRow(columns, isHeader = false)

  def columnHeaders(content: String, moreContent: String*): Grid = {
    val allContent = content :: moreContent.toList
    verifyNumValues(allContent.length)
    addContent(allContent.map(columnHeaderColor(_)))
    this.content()
  }

  def emptyLine(): Grid = content()
  def content(): Grid = allContent(List.fill(currentRow.size)(List("")))

  def content(content: Any, moreContent: Any*): Grid = {
    val allContent = content :: moreContent.toList
    verifyNumValues(allContent.length)
    addContent(allContent)
  }

  // Result of mapping function should be a Tuple
  def mapContent[T](content: Iterable[T])(f: T => Product): Grid = {
    content.map(f) foreach addTuple
    this
  }

  // Should be a list of tuples with the correct size
  def contents(content: Any): Grid = {

    def throwError() =
      throw new IllegalArgumentException(
        "Argument to contents method should be a Tuple or an Iterable[Tuple]. Received " + content
      )

    content match {
      case it: Iterable[_]  =>
        if (it.isEmpty || !it.head.isInstanceOf[Product])
          throwError()

        it.asInstanceOf[Iterable[Product]] foreach addTuple
      case product: Product => addTuple(product)
      case _                => throwError()
    }
    this
  }

  def allContent(content: Iterable[Iterable[Any]]): Grid = {
    verifyNumValues(content.size)
    content.transpose foreach addContent
    this
  }

  def columnWidths: Seq[Int] = currentRow.columnWidths

  def print(): Unit = println(render())

  def render(): String = GridRenderer().render()

  private def addRow(columns: Iterable[Column], isHeader: Boolean): Grid = {
    val row = Row(rows.length + 1, isHeader, columns.toList)
    verifyRowWidth(row)
    _currentRow = Some(row)
    rows += row
    this
  }

  private def addTuple(tuple: Product): Grid = {
    val className = tuple.getClass.getName
    if (!className.startsWith("scala.Tuple"))
      throw new IllegalArgumentException(s"Wanted a tuple but received a $className.")

    verifyNumValues(tuple.productArity)
    addContent(tuple.productIterator.toList)
  }

  private def addContent(content: Iterable[Any]) = {
    currentRow.addContent(content)
    this
  }

  private def verifyNumValues(contentLength: Int): Unit = {
    val columnLength = currentRow.columns.length
    if (contentLength != columnLength) {
      throw new IllegalArgumentException(
        s"Number of column values given to content doesn't match the number of columns in the row: $contentLength != $columnLength"
      )
    }
  }

  private def verifyRowWidth(row: Row): Unit = {
    val columns = row.columns

    val lineWidth = formatter.lineWidth
    val indentAndBorderSize = 2 + (2 * indent) + ((columns.length - 1) * (1 + 2 * indent))

    val neededWidth = indentAndBorderSize + columns
      .map(_.width)
      .collect {
        case Width.Auto    => 1 // Auto needs at least one character
        case w: FixedWidth => w(lineWidth - indentAndBorderSize)
      }
      .sum

    if (neededWidth > lineWidth)
      throw new IllegalStateException(
        s"The minimum needed width of columns in row ${ row.rowNumber } is larger than the total width: $neededWidth > $lineWidth."
      )
  }

  case class Row private(rowNumber: Int, isHeader: Boolean, columns: Seq[Column]) {

    def columnWidths: Seq[Int] = calculateColumnWidths()

    def apply(i: Int): Column = columns(i)
    def size: Int = columns.size

    def addContent(contents: Iterable[Any]): Unit = {
      calculateColumnWidths.reset()
      columns.zip(contents).foreach {
        case (column, content: GridContent) => column.addLine(content)
        case (column, content)              => column.addLine(StringContent(content.toString))
      }
    }

    private def spaceForContent(): Int = {
      val indentAndBorderSize = 2 + (2 * indent) + ((columns.length - 1) * (1 + 2 * indent))
      formatter.lineWidth - indentAndBorderSize
    }

    private val calculateColumnWidths: Memoized[Seq[Int]] = Memoize {
      val contentSpace = spaceForContent()
      val numAuto = columns.count(_.width == Width.Auto)

      if (numAuto == 0)
        columnWidthsOnlyFixedSize(contentSpace)
      else
        columnWidthsWithAuto(contentSpace, numAuto)
    }

    private def columnWidthsOnlyFixedSize(actualSpace: Int) = {
      var remainingWidth = actualSpace
      columns.zipWithIndex.map { case (column, i) =>
        val fixedWidth = column.width.asInstanceOf[FixedWidth]

        val width = if (i == columns.length - 1) remainingWidth else fixedWidth(actualSpace)

        remainingWidth -= width
        width
      }
    }

    private def columnWidthsWithAuto(contentSpace: Int, numAuto: Int) = {
      val widths = Array.fill(columns.length)(0)

      // Calculate width for the fixed columns first
      columns.zipWithIndex.foreach { case (column, i) =>
        column.width.ifInstanceOf[FixedWidth] { fixedWidth =>
          widths(i) = fixedWidth(contentSpace)
        }
      }

      val fixedWidth = widths.sum

      // We sort the columns by maximum width. We want to select the smallest
      // ones first in order to maximize the available space
      var remainingWidth = contentSpace - fixedWidth
      var remainingAutos = numAuto
      columns.zipWithIndex
        .filter(_._1.width == Width.Auto)
        .sortBy(_._1.maxWidth)
        .foreach { case (column, i) =>
          val possibleSpace = remainingWidth.toDouble / remainingAutos
          remainingAutos -= 1

          // If rounding up makes the content fit, we do that but otherwise
          // we round down. If the maxWidth of the column is less than the
          // available space we only use the needed width
          widths(i) = if (Math.ceil(possibleSpace) == column.maxWidth || column.maxWidth <= possibleSpace) {
            Math.max(1, column.maxWidth) // Never render columns completely empty
          } else {
            possibleSpace.toInt
          }
          remainingWidth -= widths(i)
        }

      // Give the last of the remaining space to the right most column with Auto width
      if (remainingWidth > 0) {
        val lastAuto = columns.lastIndexWhere(_.width == Width.Auto)
        widths(lastAuto) += remainingWidth
      }

      widths.toList
    }
  }

  private case class GridRenderer() {

    private val f = formatter

    import f._

    // An approximation of the number of characters needed. Doesn't take in to account eventual
    // line wrapping or ansi escape characters. Therefor we use double the approximated size
    private val approximateSize: Int = rows
      .map { row =>
        val numberOfRows = row.columns
          .map { column =>
            column.lines.foldLeft(0) {
              case (acc, s: StringContent) =>
                val string = s.string
                acc + math.max(1, string.length / formatter.lineWidth)
              case (acc, _)                => acc + 1
            }
          }
          .max + 1
        numberOfRows * (formatter.lineWidth + 1)
      }
      .sum

    private val sb = new StringBuilder(approximateSize)

    def render(): String = {
      if (rows.isEmpty)
        return ""

      if (drawTop) {
        sb ++= drawTopLine(rows.head)
        sb ++= NL
      }

      drawRows()
      if (drawBottom) {
        sb ++= drawBottomLine(rows.last)
      }

      sb.toString
    }

    private def drawRows(): Unit = {
      for (i <- rows.indices) {
        val row = rows(i)
        drawContent(row)

        if (i < rows.size - 1) {
          sb ++= drawMiddleLine(row, rows(i + 1))
          sb ++= NL
        }
      }
    }

    private def drawTopLine(row: Row) = {
      if (row.isHeader)
        drawTopOrBottom(TopLeftThick, HorizontalThick, HorizontalDownThick, TopRightThick, row)
      else
        drawTopOrBottom(TopLeft, Horizontal, HorizontalDown, TopRight, row)
    }

    private def drawBottomLine(row: Row) = {
      if (row.isHeader)
        drawTopOrBottom(BottomLeftThick, HorizontalThick, HorizontalUpThick, BottomRightThick, row)
      else
        drawTopOrBottom(BottomLeft, Horizontal, HorizontalUp, BottomRight, row)
    }

    private def drawTopOrBottom(left: String, middle: String, middleBreak: String, right: String, row: Row): String = {
      val sb = new StringBuilder
      sb ++= left
      row.columnWidths.zipWithIndex.foreach { case (width, index) =>
        val num = 2 * indent + width
        sb ++= middle * num
        if (index != row.columns.size - 1)
          sb ++= middleBreak
      }

      sb ++= right
      borderColor(sb.toString)
    }

    private def drawMiddleLine(before: Row, after: Row) = {
      // Returns the positions where the row has a column break
      def getBreakPositions(row: Row): Iterator[Int] = {
        val widths = row.columnWidths
        var acc = 2 * indent + widths.head
        widths.tail.map { width => acc use { _ => acc += width + 2 * indent + 1 } }.iterator
      }

      val (horizontalVertical, horizontalUp, horizontalDown, horizontal, verticalRight, verticalLeft) =
        if (before.isHeader)
          (HorizontalVerticalThick, HorizontalUpThick, HorizontalDownThick, HorizontalThick, VerticalRightThick, VerticalLeftThick)
        else
          (HorizontalVertical, HorizontalUp, HorizontalDown, Horizontal, VerticalRight, VerticalLeft)

      val sb = new StringBuilder
      val maxWidth = formatter.lineWidth

      sb ++= verticalRight

      val upPositions = getBreakPositions(before)
      val downPositions = getBreakPositions(after)
      var up = if (upPositions.hasNext) upPositions.next() else -1
      var down = if (downPositions.hasNext) downPositions.next() else -1
      var x = 0
      while (x < maxWidth - 2) {
        val X = x // X is a stable identifier, we cant use x since it's a var
        sb ++= ((up, down) match {
          case (X, X) => horizontalVertical
          case (X, _) => horizontalUp
          case (_, X) => horizontalDown
          case _      => horizontal
        })

        if (upPositions.hasNext && x >= up)
          up = upPositions.next()
        if (downPositions.hasNext && x >= down)
          down = downPositions.next()

        x += 1
      }
      sb ++= verticalLeft
      borderColor(sb.toString)
    }

    private def drawContent(row: Row): Unit = {
      val columns = row.columns
      val columnWidths = row.columnWidths

      val rowContent = columns
        .map(column => if (column.lines.nonEmpty) column.lines else List(StringContent("")))
        // Transpose to get the corresponding line in each column together
        .transpose
        .map { lines =>
          // Handle overflow in each line
          val linesWithIndex = lines.zipWithIndex

          val overflowedLines = linesWithIndex.map { case (line, columnIndex) =>
            val width = columnWidths(columnIndex)
            val handleOverflow = columns(columnIndex).overflowHandling
            handleOverflow(line.render(width), width)
          }
          val maxNumLines = overflowedLines.map { _.length }.max
          if (maxNumLines > 1) {
            // Fill out the columns that didn't word wrap
            // so that each column has the same number of lines after word wrapping
            val overflowedContent = linesWithIndex.map { case (content, columnIndex) =>
              val width = columnWidths(columnIndex)
              content.overflowedLineContent(width)
            }
            overflowedLines
              .zip(overflowedContent)
              .map { case (lines, overflowContent) =>
                lines ::: List.fill(maxNumLines - lines.size)(overflowContent)
              }
          } else {
            overflowedLines
          }
        }
        .transpose // Transpose back so that we have a list of list of lines for each column
        .map(_.flatten) // Flatten the list so we just have one list of lines for each column
        // Transpose to get the lists of each line to draw
        .transpose
        .map { columnsInLine =>
          // Draw each line
          val content = columnsInLine.zipWithIndex.map { case (line, columnIndex) =>
            columns(columnIndex).alignment(line, columnWidths(columnIndex))
          }
          val fill = " " * indent
          val columnBreak = borderColor(Vertical)
          val line = columnBreak + fill + content.mkString(fill + columnBreak + fill) + fill + columnBreak
          line
        }
        .mkString(NL)
      sb ++= rowContent
      sb ++= NL
    }
  }

}
