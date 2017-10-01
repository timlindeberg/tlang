package tlang.repl

import tlang.formatting.Colors.Color
import tlang.formatting.Formatter
import tlang.formatting.grid.TruncatedColumn
import tlang.formatting.textformatters.Marking
import tlang.messages.{CompilerMessage, MessageFormatter}
import tlang.repl.input.{Cursor, InputBuffer}
import tlang.repl.terminal.ReplTerminal
import tlang.utils.Extensions._
import tlang.utils.{Position, Positioned}

import scala.concurrent.duration.FiniteDuration

object OutputBox {

  val YIndent  = 3
  val XIndent  = 2
  val TabWidth = 3

}

class OutputBox(
  formatter: Formatter,
  errorFormatter: MessageFormatter,
  terminal: ReplTerminal,
  maxOutputLines: Int) {


  private val formatting = formatter.formatting

  import OutputBox._
  import formatting._

  private val SuccessColor = Bold + Green
  private val ErrorColor   = Bold + Red
  private val InputColor   = Bold + Magenta

  private val MarkColor = Inverse

  private val TabReplacement    = " " * TabWidth
  private val ShowCtrlCReminder = FiniteDuration(2, "sec")
  private val spinner           = formatting.spinner

  private var input            = ""
  private var renderedText     = ""
  private var tabsBeforeCursor = 0
  private var lineLength       = 0

  private var result            = Seq[Seq[String]]()
  private var cursor            = Cursor()
  private var previousBoxHeight = 0
  private var boxHeight         = 0
  private var header            = InputColor("Input")
  private var isFinished        = false


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

    renderedText = formatter.syntaxHighlight(replacedTabs, Marking(adjustedPos, MarkColor, isAdditive = true))
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

    terminal.isCursorVisible = false
    terminal.putBox(grid, resetStartPosition = !isFinished)

    val heightDifference = previousBoxHeight - boxHeight
    if (heightDifference > 0)
      terminal.clearScreenFromCursorPosition()

    previousBoxHeight = boxHeight


    if (!isFinished) {
      // Reset position to beginning of box
      val newPos = terminal.boxStartPosition
        .withRelativeRow(YIndent + cursor.y)
        .withRelativeColumn(XIndent + cursor.x + (TabWidth - 1) * tabsBeforeCursor)
      terminal.setCursorPosition(newPos)
    }

    // To make room for truncation
    val boxSpace = formatting.lineWidth - 2 * XIndent
    val end = if (lineLength > boxSpace) boxSpace - 3 else boxSpace
    val isCursorInsideBox = cursor.x <= end
    terminal.isCursorVisible = isCursorInsideBox && isCursorVisible
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
    val clearLine = (" " * formatting.lineWidth) + NL
    terminal.put(clearLine * num)
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