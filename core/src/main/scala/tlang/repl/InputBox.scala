package tlang.repl

import tlang.compiler.error.{ErrorFormatter, ErrorMessage, Formatting, Marking}
import tlang.utils.Colors.Color

import scala.concurrent.duration.{Duration, FiniteDuration}

abstract class Spinner(characters: IndexedSeq[Char], val frameTime: FiniteDuration) {

  private var index        = 0
  private var _elapsedTime = Duration(0, "ms")

  def nextCharacter: Char = {
    val c = characters(index)
    index = (index + 1) % characters.size
    _elapsedTime += frameTime
    c
  }

  def elapsedTime: FiniteDuration = _elapsedTime
  def reset(): Unit = index = 0

}

case class BrailSpinner() extends Spinner("⣾⣽⣻⢿⡿⣟⣯⣷", FiniteDuration(200, "ms"))
case class ASCIISpinner() extends Spinner("|/—\\\\", FiniteDuration(200, "ms"))
class InputBox(formatting: Formatting, maxOutputLines: Int, val terminal: ReplTerminal) {

  import formatting._
  import formatting.colors._

  private val SuccessColor      = Bold + Green
  private val ErrorColor        = Bold + Red
  private val MarkedColor       = WhiteBG + Black
  private val InputColor        = Bold + Magenta
  private val YIndent           = 3
  private val XIndent           = 2
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
    var text = Bold(spinner.nextCharacter)
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

    terminal.setCursorVisible(true)
  }

  private def setResult(output: String, shouldTruncate: Boolean, color: Color, headerText: String): Unit = {
    if (isFinished)
      return

    isFinished = true
    header = color(headerText)
    val txt = if (shouldTruncate) truncate(output, color) else output
    result = divider + makeLines(txt) + bottom
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

  private def highlight(text: String) = if (text.startsWith(":")) InputColor(text) else syntaxHighlighter(text)


}
