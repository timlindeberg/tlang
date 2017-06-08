package tlang.repl

import tlang.compiler.error.{ErrorFormatter, ErrorMessage}
import tlang.repl.input.{Cursor, InputBuffer}
import tlang.utils.formatting.Colors.Color
import tlang.utils.formatting.{Formatting, Marking}

import scala.concurrent.duration.FiniteDuration

class InputBox(formatting: Formatting, maxOutputLines: Int, val terminal: ReplTerminal) {

  import formatting._

  private val SuccessColor      = Bold + Green
  private val ErrorColor        = Bold + Red
  private val MarkedColor       = WhiteBG + Black
  private val InputColor        = Bold + Magenta
  private val YIndent           = 3
  private val XIndent           = 2
  private val TabWidth          = 1
  private val TabReplacement    = " " * TabWidth
  private val ShowCtrlCReminder = FiniteDuration(2, "sec")
  private val spinner           = formatting.spinner

  private var inputText           = ""
  private var result              = ""
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
    result = divider + makeLine(text) + bottom
  }


  def newInput(inputBuffer: InputBuffer): Unit = {
    if (isFinished)
      return

    cursor = inputBuffer.mainCursor
    val input = inputBuffer.toString
    val text = if (input.trim.startsWith(":")) InputColor(input) else input
    inputText = syntaxHighlighter(text, Marking(inputBuffer.getMarkedPosition, MarkedColor))
      .replaceAll("\t", TabReplacement)
    boxHeight = inputBuffer.height
    render()
  }


  def exit(): Unit = {
    if (isFinished)
      return

    isFinished = true
    val text = Bold("Thanks for using the ") + SuccessColor("T-REPL") + Bold("!")

    result = divider + makeLines(text) + bottom
  }

  def success(output: String, truncate: Boolean): Unit = setResult(output, truncate, SuccessColor, "Result")
  def failure(output: String, truncate: Boolean): Unit = setResult(output, truncate, ErrorColor, "Error")
  def compileError(errors: List[ErrorMessage]): Unit = {
    if (isFinished)
      return

    isFinished = true
    header = ErrorColor("Error")

    val markings = errors.map { error => Marking(error.pos, Bold + Underline + Red) }
    inputText = syntaxHighlighter(inputText, markings)
      .replaceAll("\t", TabReplacement)

    val errorLines = errors.map { error =>
      val errorFormatter = ErrorFormatter(error, formatting, errorContextSize = 0)
      (errorFormatter.position, errorFormatter.errorPrefix + error.message)
    }

    val diff = errorLines.size - maxOutputLines
    val lines = errorLines.take(maxOutputLines)
    val truncated = if (diff <= 0) lines else lines :+ ("", ErrorColor(s"... $diff more"))

    result = makeBlockWithColumn(truncated, endOfBlock = true)
  }

  def render(): Unit = {
    val sb = new StringBuilder
    sb ++= makeHeader(header)
    sb ++= divider
    sb ++= makeLines(inputText)
    sb ++= (if (result == "") bottom else result)

    val isCursorVisible = terminal.isCursorVisible

    terminal.setCursorVisible(false)
    var linesPut = putBox(sb.toString)

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
    val txt = if (shouldTruncate) truncate(output, color) else output

    if (txt.isEmpty) {
      result = bottom
    } else {
      header = color(headerText)
      result = divider + makeLines(txt) + bottom
    }
  }

  private def clearLines(num: Int): Unit = {
    val clearLine = (" " * formatting.lineWidth) + "\n"
    terminal.put(clearLine * num)
  }

  private def putBox(chars: IndexedSeq[Char]): Int = {
    terminal.setCursorPosition(boxStartingPosition)
    val linesPut = terminal.put(chars)
    boxStartingPosition = terminal.getCursorPosition
    linesPut
  }

  private def truncate(output: String, color: Color): String = {
    val wordWrapped = wordWrapper(output, formatting.lineWidth, maxLines = maxOutputLines)

    val diff = wordWrapped.size - maxOutputLines
    val lines = wordWrapped.take(maxOutputLines)
    val truncated = if (diff <= 0) lines else lines :+ color(s"... $diff more")
    truncated.mkString("\n")
  }

}
