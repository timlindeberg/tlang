package tlang.utils.formatting.grid

import tlang.utils.Extensions._
import tlang.utils.formatting.Colors.Color
import tlang.utils.formatting._

import scala.collection.mutable.ListBuffer


case class Grid(var formatting: Formatting) {

  private val rows: ListBuffer[Row] = ListBuffer()
  private var indent                = 1
  private var borderColor           = formatting.NoColor
  private var shouldTrim            = true

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

  def formatting(formatting: Formatting): Grid = {
    this.formatting = formatting
    this
  }

  def indent(indent: Int): Grid = {
    this.indent = indent
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

  def row(
    width: Width = Width.Auto,
    handleOverflow: OverflowHandling = OverflowHandling.Wrap,
    alignment: Alignment = Alignment.Left
  ): Grid =
    row(Column(width, alignment, handleOverflow))

  def row(numColumns: Int): Grid = row(List.fill(numColumns)(Column.copy()))

  def row(column: Column, moreColumns: Column*): Grid = {
    // This allows us to pass the Column object to receive a column with default values
    // That way we can write .row(Column, Column) instead of .row(Column(), Column())
    val columns = (column :: moreColumns.toList) map {
      case Column => Column.copy()
      case c      => c
    }
    row(columns)
  }

  def row(columns: Iterable[Column]): Grid = addRow(columns, isHeader = false)

  private def addRow(columns: Iterable[Column], isHeader: Boolean): Grid = {
    val row = Row(rows.length + 1, isHeader, columns.toList)
    verifyRowWidth(row)
    _currentRow = Some(row)
    rows += row
    this
  }

  def content(): Grid = allContent(List.fill(currentRow.size)(List("")))

  // Yay!
  def content(t: (String, String)): Grid = addTuple(t)
  def content(t: (String, String, String)): Grid = addTuple(t)
  def content(t: (String, String, String, String)): Grid = addTuple(t)
  def content(t: (String, String, String, String, String)): Grid = addTuple(t)
  def content(t: (String, String, String, String, String, String)): Grid = addTuple(t)
  def content(t: (String, String, String, String, String, String, String)): Grid = addTuple(t)
  def content(t: (String, String, String, String, String, String, String, String)): Grid = addTuple(t)
  def content(t: (String, String, String, String, String, String, String, String, String)): Grid = addTuple(t)
  def content(t: (String, String, String, String, String, String, String, String, String, String)): Grid = addTuple(t)
  def content(t: (String, String, String, String, String, String, String, String, String, String, String)): Grid = addTuple(t)
  def content(t: (String, String, String, String, String, String, String, String, String, String, String, String)): Grid = addTuple(t)
  def content(t: (String, String, String, String, String, String, String, String, String, String, String, String, String)): Grid = addTuple(t)
  def content(t: (String, String, String, String, String, String, String, String, String, String, String, String, String, String)): Grid = addTuple(t)
  def content(t: (String, String, String, String, String, String, String, String, String, String, String, String, String, String, String)): Grid = addTuple(t)
  def content(t: (String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String)): Grid = addTuple(t)
  def content(t: (String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String)): Grid = addTuple(t)
  def content(t: (String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String)): Grid = addTuple(t)
  def content(t: (String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String)): Grid = addTuple(t)
  def content(t: (String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String)): Grid = addTuple(t)
  def content(t: (String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String)): Grid = addTuple(t)
  def content(t: (String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String)): Grid = addTuple(t)


  def content(content: String, moreContent: String*): Grid = {
    val allContent = content :: moreContent.toList
    verifyNumValues(allContent.length)
    addContent(allContent)
  }

  // Result of mapping function should be a Tuple
  def mapContent[T](content: Iterable[T])(f: (T) => Product): Grid = {
    content.map(f) foreach addTuple
    this
  }

  // Should be a list of tuples with the correct size
  def contents(content: Iterable[Product]): Grid = {
    content foreach addTuple
    this
  }

  def allContent(content: Iterable[Iterable[Any]]): Grid = {
    verifyNumValues(content.size)
    content.transpose foreach addContent
    this
  }

  def columnWidths: Seq[Int] = currentRow.columnWidths

  def print(): Unit = println(toString)

  override def toString: String = GridRenderer(formatting).render()

  private def addTuple(tuple: Product): Grid = {
    val className = tuple.getClass.getName
    if (!className.startsWith("scala.Tuple"))
      throw new IllegalArgumentException(s"Wanted a tuple but received a $className.")

    verifyNumValues(tuple.productArity)
    addContent(tuple.productIterator.toList)
  }

  private def addContent(content: Iterable[Any]) = {
    currentRow.columnWidthsDirty = true
    currentRow.columns.zip(content).foreach { case (column, content) =>
      column.addLine(content.toString)
    }
    this
  }

  private def verifyNumValues(contentLength: Int) = {
    val columnLength = currentRow.columns.length
    if (contentLength != columnLength) {
      throw new IllegalArgumentException(
        s"Number of column values given to content doesn't match the number of columns in the row: $contentLength != $columnLength"
      )
    }
  }

  private def verifyRowWidth(row: Row) = {
    val columns = row.columns

    val lineWidth = formatting.lineWidth
    val indentAndBorderSize = 2 + (2 * indent) + ((columns.length - 1) * (1 + 2 * indent))

    val neededWidth = indentAndBorderSize + columns
      .map(_.width)
      .collect {
        case Width.Auto    => 1 // Auto needs at least one character
        case w: FixedWidth => w(lineWidth - indentAndBorderSize)
      }
      .sum

    if (neededWidth > lineWidth)
      throw new IllegalStateException(s"The minimum needed width of columns in row ${ row.rowNumber } is larger than the total width: $neededWidth > $lineWidth.")
  }

  case class Row private(rowNumber: Int, isHeader: Boolean, columns: Seq[Column]) {

    private[grid] var columnWidthsDirty       = true
    private       var _columnWidths: Seq[Int] = Seq()

    def apply(i: Int): Column = columns(i)
    def size: Int = columns.size

    def columnWidths: Seq[Int] = {
      if (columnWidthsDirty) {
        _columnWidths = calculateColumnWidths()
        columnWidthsDirty = false
      }
      _columnWidths
    }

    private def spaceForContent(): Int = {
      val indentAndBorderSize = 2 + (2 * indent) + ((columns.length - 1) * (1 + 2 * indent))
      formatting.lineWidth - indentAndBorderSize
    }

    private def calculateColumnWidths(): Seq[Int] = {
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
        val width = if (i == columns.length - 1)
          remainingWidth
        else
          fixedWidth(actualSpace)

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

  case class GridRenderer(formatting: Formatting) {

    import formatting.boxStyle._

    val sb = new StringBuilder()

    def render(): String = {
      if (rows.isEmpty)
        return ""

      var i = 0

      sb ++= drawTopLine(rows.head)
      sb ++= "\n"
      while (i < rows.size) {
        val row = rows(i)
        drawContent(row)

        if (i < rows.size - 1) {
          sb ++= drawMiddleLine(row, rows(i + 1))
          sb ++= "\n"
        }
        i += 1
      }
      sb ++= drawBottomLine(rows.last)
      sb.toString
    }

    private def drawTopLine(row: Row) = {
      if (row.isHeader)
        drawTopOrBottom(╒, ═, ╤, ╕, row)
      else
        drawTopOrBottom(┌, ─, ┬, ┐, row)
    }

    private def drawBottomLine(row: Row) = {
      if (row.isHeader)
        drawTopOrBottom(╘, ═, ╧, ╛, row)
      else
        drawTopOrBottom(└, ─, ┴, ┘, row)

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
      formatLine(sb.toString)
    }

    private def drawMiddleLine(before: Row, after: Row) = {
      // Returns the positions where the row has a column break
      def getBreakPositions(row: Row): Iterator[Int] = {
        val widths = row.columnWidths
        var acc = 2 * indent + widths.head
        widths.tail.map { width => acc use { _ => acc += width + 2 * indent + 1 } }.iterator
      }
      def ifHeader(a: String, b: String) = if (before.isHeader) a else b

      val sb = new StringBuilder
      val maxWidth = formatting.lineWidth

      sb ++= ifHeader(╞, ├)

      val upPositions = getBreakPositions(before)
      val downPositions = getBreakPositions(after)
      var up = if (upPositions.hasNext) upPositions.next() else -1
      var down = if (downPositions.hasNext) downPositions.next() else -1
      var x = 0
      while (x < maxWidth - 2) {
        val X = x // X is a stable identifier, we cant use x since it's a var
        sb ++= ((up, down) match {
          case (X, X) => ifHeader(╪, ┼)
          case (X, _) => ifHeader(╧, ┴)
          case (_, X) => ifHeader(╤, ┬)
          case _      => ifHeader(═, ─)
        })

        if (upPositions.hasNext && x >= up)
          up = upPositions.next()
        if (downPositions.hasNext && x >= down)
          down = downPositions.next()

        x += 1
      }
      sb ++= ifHeader(╡, ┤)
      formatLine(sb.toString)
    }

    private def formatLine(line: String) = borderColor(trimRight(line))

    private def drawContent(row: Row): Unit = {
      val columns = row.columns
      val columnWidths = row.columnWidths


      sb ++= columns
        .map(column => if (column.lines.nonEmpty) column.lines else List(""))
        // Transpose to get the corresponding line in each column together
        .transpose
        .map { lines =>
          // Word wrap each line in each column
          val wrappedLines = lines.zipWithIndex.map { case (line, columnIndex) =>
            columns(columnIndex).overflowHandling(line, columnWidths(columnIndex))
          }
          val maxNumLines = wrappedLines.map(_.length).max


          // Fill out the columns with empty lines so that each column has the same
          // number of lines after word wrapping
          wrappedLines.map { lines => lines ::: List.fill(maxNumLines - lines.size)("") }
        }
        .transpose // Transpose back so that we have a list of list of lines for each column
        .map(_.flatten) // Flatten the list so we just have one list of lines for each column
        // Transpose to get the lists of each line to draw
        .transpose
        .map { lines =>
          // Draw each line
          val content = lines.zipWithIndex.map { case (line, columnIndex) =>
            columns(columnIndex).alignment(line, columnWidths(columnIndex))
          }
          val fill = " " * indent
          val columnBreak = borderColor(│)
          trimRight(columnBreak + fill + content.mkString(fill + columnBreak + fill) + fill + columnBreak)
        }
        .mkString("\n")
      sb ++= "\n"
    }

    private def trimRight(s: String) = if (shouldTrim) s.rightTrimWhiteSpaces else s
  }

}
