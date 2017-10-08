package tlang.repl

import tlang.formatting.Colors.Color
import tlang.formatting.grid.TruncatedColumn
import tlang.formatting.textformatters.{Marking, TabReplacer}
import tlang.formatting.{Formatter, Spinner}
import tlang.messages.{CompilerMessage, MessageFormatter}
import tlang.repl.input.InputBuffer
import tlang.utils.Extensions._

import scala.concurrent.duration.FiniteDuration


case class RenderState(
  input: String = "",
  highlightedInput: String = "",
  header: String = "",
  content: Seq[Seq[String]] = Seq()
) {
  def contentAsString: String = content.flatMap(_.mkString).mkString
}

object OutputBox {

  val YIndent           = 3
  val XIndent           = 2
  val ShowCtrlCReminder = FiniteDuration(2, "sec")

  def apply(formatter: Formatter, tabReplacer: TabReplacer, errorFormatter: MessageFormatter, maxOutputLines: Int): OutputBox = {
    val formatting = formatter.formatting
    import formatting._

    val inputColor = Bold + Magenta
    val header = inputColor("Input")
    val renderState = RenderState(header = header)
    new OutputBox(
      formatter,
      errorFormatter,
      tabReplacer,
      maxOutputLines,
      renderState,
      formatting.spinner
    )
  }

}

case class OutputBox private(
  formatter: Formatter,
  errorFormatter: MessageFormatter,
  tabReplacer: TabReplacer,
  maxOutputLines: Int,
  renderState: RenderState,
  spinner: Spinner
) {

  private val formatting = formatter.formatting

  import OutputBox._
  import formatting._

  private lazy val InputColor   = Bold + Magenta
  private lazy val SuccessColor = Bold + Green
  private lazy val ErrorColor   = Bold + Red
  private lazy val MarkColor    = Inverse

  def clear(): OutputBox = {
    val header = InputColor("Input")
    copy(renderState = RenderState(header = header), spinner = formatting.spinner)
  }

  def welcome(): OutputBox = {
    val commands = List("help", "quit", "print").map(command => Magenta(s":$command"))
    val commandList = formatter.list(commands)
    val evaluateColor = Bold + Blue
    val exitColor = Bold + Red
    val tColor = Bold + Green

    copy(renderState = renderState.copy(
      header = Bold("Welcome to the ") + tColor("T-REPL") + Bold("!"),
      content = List(List(
        s"""|Type in code to have it evaluated or type one of the following commands:
            |
            |$commandList
            |
            |Press ${ evaluateColor("CTRL") } + ${ evaluateColor("Space") } to evaluate the input and ${ exitColor("CTRL") } + ${ exitColor("C") } to exit or type ${ Magenta(":quit") }.
          """.stripMargin.trim
      ))
    ))
  }


  def exit(): OutputBox = {
    val content = Bold("Thanks for using the ") + SuccessColor("T-REPL") + Bold("!")
    copy(renderState = renderState.copy(content = List(List(content))))
  }

  def nextLoadingState(): OutputBox = {
    var text = Bold(spinner.nextImage)
    if (spinner.elapsedTime > ShowCtrlCReminder)
      text += InputColor("   Press Ctrl+C to cancel execution.")
    copy(renderState = renderState.copy(content = List(List(text))))
  }

  def newInput(inputBuffer: InputBuffer): OutputBox = {
    val input = inputBuffer.toString

    val text = if (input.trim.startsWith(":")) InputColor(input) else input
    // Selected position is already adjusted for tabs so we can just replace it like normal.
    val replacedTabs = text.replaceAll("\t", " " * tabReplacer.tabWidth)

    val selectionMarking = Marking(inputBuffer.selectedPosition, MarkColor, isAdditive = true)
    val highlightedInput = formatter.syntaxHighlight(replacedTabs, selectionMarking)
    copy(
      renderState = renderState.copy(input = input, highlightedInput = highlightedInput)
    )
  }


  def success(output: String, truncate: Boolean): OutputBox = setResult(output, truncate, SuccessColor, "Result")
  def failure(output: String, truncate: Boolean): OutputBox = setResult(output, truncate, ErrorColor, "Error")
  def compileError(errors: Seq[CompilerMessage]): OutputBox = {
    val header = ErrorColor("Error")

    val errorPositions = errors.map(_.pos)
    val (replacedTabs, adjustedPositions) = tabReplacer(renderState.input, errorPositions)

    val markings = adjustedPositions.map { pos => Marking(pos, Bold + Underline + Red) }
    val highlightedInput = formatter.syntaxHighlight(replacedTabs, markings)

    val errorLines = errors.map { error =>
      errorFormatter.setMessage(error)
      val pos = error.pos
      val locationIndicator = NumColor(pos.line) + ":" + NumColor(pos.col)
      (locationIndicator, errorFormatter.prefix + " " + error.message)
    }

    val diff = errorLines.size - maxOutputLines
    val lines = errorLines.take(maxOutputLines)
    val truncated = if (diff <= 0) lines else lines :+ ("", ErrorColor(s"... $diff more"))
    val unzipped = truncated.unzip
    val content = List(unzipped._1, unzipped._2)
    copy(renderState = renderState.copy(highlightedInput = highlightedInput, header = header, content = content))
  }

  def render(): String = {
    val grid = formatter
      .grid
      .header(renderState.header)
      .row(TruncatedColumn)
      .content(renderState.highlightedInput)

    val result = renderState.content
    if (result.nonEmpty)
      grid.row(result.size).allContent(result)
    grid.render() + NL
  }

  private def setResult(output: String, shouldTruncate: Boolean, color: Color, headerText: String): OutputBox = {
    val text = if (shouldTruncate) truncate(output, color) else output
    val colored = formatter.syntaxHighlight(text)
    val header = color(headerText)
    val content = List(List(colored))

    copy(renderState = renderState.copy(header = header, content = content))
  }

  private def truncate(output: String, color: Color): String = {
    val wordWrapped = formatter.wrap(output, formatting.lineWidth)

    val diff = wordWrapped.size - maxOutputLines
    val lines = wordWrapped.take(maxOutputLines)
    val truncated = if (diff <= 0) lines else lines :+ color(s"... $diff more")
    truncated.mkString("\n")
  }

}
