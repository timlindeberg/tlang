package tlang.repl

import tlang.formatting.Colors.Color
import tlang.formatting.Formatter
import tlang.formatting.grid.Grid
import tlang.formatting.textformatters.Marking
import tlang.messages.{CompilerMessage, MessageFormatter}
import tlang.repl.input.{Cursor, InputBuffer}

import scala.concurrent.duration.FiniteDuration

class InputBox(
  formatter: Formatter,
  errorFormatter: MessageFormatter,
  maxOutputLines: Int,
  terminal: ReplTerminal) {

  private val formatting = formatter.formatting

  import formatting._

  private val SuccessColor      = Bold + Green
  private val ErrorColor        = Bold + Red
  private val InputColor        = Bold + Magenta
  private val YIndent           = 3
  private val XIndent           = 2
  private val TabWidth          = 1
  private val TabReplacement    = " " * TabWidth
  private val ShowCtrlCReminder = FiniteDuration(2, "sec")
  private val spinner           = formatting.spinner

  private var inputText           = ""
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
    val input = inputBuffer.toString
    val text = if (input.trim.startsWith(":")) InputColor(input) else input
    inputText = formatter.syntaxHighlight(text, Marking(inputBuffer.selectedPosition, Inverse))
      .replaceAll("\t", TabReplacement)
    boxHeight = inputBuffer.height
    render()
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

    val markings = errors.map { error => Marking(error.pos, Bold + Underline + Red) }
    inputText = formatter.syntaxHighlight(inputText, markings).replaceAll("\t", TabReplacement)

    val errorLines = errors.map { error =>
      errorFormatter.setMessage(error)
      (errorFormatter.positionDescription, errorFormatter.prefix + " " + error.message)
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
      .row()
      .content(inputText)

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
      val newPos = boxStartingPosition.withRelativeRow(YIndent + cursor.y).withRelativeColumn(XIndent + cursor.x)
      terminal.setCursorPosition(newPos)
    }

    if (isCursorVisible)
      terminal.setCursorVisible(true)
  }

  private def setResult(output: String, shouldTruncate: Boolean, color: Color, headerText: String): Unit = {
    if (isFinished)
      return

    isFinished = true
    val text = if (shouldTruncate) truncate(output, color) else output

    if (text.nonEmpty) {
      header = color(headerText)
      setResult(text)
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

}
