package tlang.repl

import tlang.formatting.Colors.Color
import tlang.formatting.Formatter
import tlang.formatting.grid.{Grid, TruncatedColumn}
import tlang.formatting.textformatters.Marking
import tlang.messages.{CompilerMessage, MessageFormatter}
import tlang.repl.input.{Cursor, InputBuffer}
import tlang.utils.Extensions._
import tlang.utils.{Position, Positioned}

import scala.concurrent.duration.FiniteDuration

class InputBox(
  formatter: Formatter,
  errorFormatter: MessageFormatter,
  maxOutputLines: Int,
  terminal: ReplTerminal) {

  private val formatting = formatter.formatting

  import formatting._

  private val SuccessColor = Bold + Green
  private val ErrorColor   = Bold + Red
  private val InputColor   = Bold + Magenta

  // Using inverse could be cool but it seems Lanterna doesn't handle it properly
  private val MarkColor         = WhiteBG + Black
  private val YIndent           = 3
  private val XIndent           = 2
  private val TabWidth          = 3
  private val TabReplacement    = " " * TabWidth
  private val ShowCtrlCReminder = FiniteDuration(2, "sec")
  private val BoxSpace          = formatting.lineWidth - 2 * XIndent
  private val spinner           = formatting.spinner

  private var input               = ""
  private var renderedText        = ""
  private var tabsBeforeCursor    = 0
  private var lineLength          = 0

  private var result              = Seq[Seq[String]]()
  private var cursor              = Cursor()
  private var boxStartingPosition = terminal.getCursorPosition
  private var previousBoxHeight   = 0
  private var boxHeight           = 0
  private var header              = InputColor("Input")
  private var isFinished          = false


  def nextLoadingState(): Unit = {
    var text = Bold(spinner.nextImage)
    if (spinner.elapsedTime > ShowCtrlCReminder)
      text += InputColor("   Press Ctrl+C to cancel execution.")
    setResult(text)
  }


  def newInput(inputBuffer: InputBuffer): Unit = {
    if (isFinished)
      return


    cursor = inputBuffer.mainCursor
    val currentLine = inputBuffer.currentLine
    tabsBeforeCursor = currentLine.take(cursor.x).count(_ == '\t')
    lineLength = currentLine.length

    input = inputBuffer.toString

    val text = if (input.trim.startsWith(":")) InputColor(input) else input
    val (replacedTabs, adjustedPos) = replaceTabs(text, inputBuffer.selectedPosition)

    renderedText = formatter.syntaxHighlight(replacedTabs, Marking(adjustedPos, MarkColor))
    boxHeight = inputBuffer.height
  }


  def exit(): Unit = {
    if (isFinished)
      return

    isFinished = true
    setResult(Bold("Thanks for using the ") + SuccessColor("T-REPL") + Bold("!"))
  }

  def success(output: String, truncate: Boolean): Unit = setResult(output, truncate, SuccessColor, "Result")
  def failure(output: String, truncate: Boolean): Unit = setResult(output, truncate, ErrorColor, "Error")
  def compileError(errors: Seq[CompilerMessage]): Unit = {
    if (isFinished)
      return

    isFinished = true
    header = ErrorColor("Error")

    val errorPositions = errors.map(_.pos)
    val (replacedTabs, adjustedPositions) = replaceTabs(input, errorPositions)

    val markings = adjustedPositions.map { pos => Marking(pos, Bold + Underline + Red) }
    renderedText = formatter.syntaxHighlight(replacedTabs, markings)

    val errorLines = errors.map { error =>
      errorFormatter.setMessage(error)
      (NumColor(error.pos.line), errorFormatter.prefix + " " + error.message)
    }

    val diff = errorLines.size - maxOutputLines
    val lines = errorLines.take(maxOutputLines)
    val truncated = if (diff <= 0) lines else lines :+ ("", ErrorColor(s"... $diff more"))
    setResult(truncated)
  }

  def render(): Unit = {
    val grid = formatter
      .grid
      .header(header)
      .row(TruncatedColumn)
      .content(renderedText)

    if (result.nonEmpty)
      grid.row(result.size).allContent(result)

    val isCursorVisible = terminal.isCursorVisible

    terminal.setCursorVisible(false)
    var linesPut = putGrid(grid)

    val heightDifference = previousBoxHeight - boxHeight
    if (heightDifference > 0) {
      clearLines(heightDifference)
      linesPut += heightDifference
    }

    previousBoxHeight = boxHeight


    if (!isFinished) {
      // Reset position to beginning of box
      boxStartingPosition = terminal.getCursorPosition.withRelativeRow(-linesPut).withColumn(0)
      val newPos = boxStartingPosition
        .withRelativeRow(YIndent + cursor.y)
        .withRelativeColumn(XIndent + cursor.x + (TabWidth - 1) * tabsBeforeCursor)
      terminal.setCursorPosition(newPos)
    }
    // To make room for truncation
    val end = if(lineLength > BoxSpace) BoxSpace - 3 else BoxSpace
    val isCursorInsideBox = cursor.x <= end
    terminal.setCursorVisible(isCursorInsideBox && isCursorVisible)
  }

  private def setResult(output: String, shouldTruncate: Boolean, color: Color, headerText: String): Unit = {
    if (isFinished)
      return

    isFinished = true
    val text = if (shouldTruncate) truncate(output, color) else output
    val colored = formatter.syntaxHighlight(text)

    if (colored.nonEmpty) {
      header = color(headerText)
      setResult(colored)
    }
  }

  private def setResult(text: String): Unit = {
    result = List(List(text))
  }

  private def setResult(content: Seq[(String, String)]): Unit = {
    val unzipped = content.unzip
    result = List(unzipped._1, unzipped._2)
  }

  private def clearLines(num: Int): Unit = {
    val clearLine = (" " * formatting.lineWidth) + "\n"
    terminal.put(clearLine * num)
  }

  private def putGrid(grid: Grid): Int = {
    terminal.setCursorPosition(boxStartingPosition)
    val linesPut = terminal.put(grid.render + "\n")
    boxStartingPosition = terminal.getCursorPosition
    linesPut
  }

  private def truncate(output: String, color: Color): String = {
    val wordWrapped = formatter.wrap(output, formatting.lineWidth)

    val diff = wordWrapped.size - maxOutputLines
    val lines = wordWrapped.take(maxOutputLines)
    val truncated = if (diff <= 0) lines else lines :+ color(s"... $diff more")
    truncated.mkString("\n")
  }


  private def replaceTabs(text: String, pos: Positioned): (String, Positioned) = {
    val (t, p) = replaceTabs(text, Seq(pos))
    (t, p.head)
  }

  private def replaceTabs(text: String, positionsToAlter: Seq[Positioned]): (String, Seq[Positioned]) = {
    var positions = positionsToAlter
    val lines = text.split(NL, -1)
    var sb = new StringBuilder

    lines.zipWithIndex foreach { case (line, lineNum) =>
      if (lineNum > 0)
        sb ++= NL

      for (i <- 0 until line.length) {
        val c = line(i)
        c match {
          case '\t' =>
            positions = positions.map { pos =>
              var start = pos.col
              var end = pos.endCol
              if (lineNum + 1 == pos.line && start - 1 > i) start += TabWidth - 1
              if (lineNum + 1 == pos.endLine && end - 1 >= i) end += TabWidth - 1
              Position(pos.line, start, pos.endLine, end)
            }

            sb ++= TabReplacement
          case _    =>
            sb += c
        }
      }
    }
    (sb.toString, positions)
  }

}
