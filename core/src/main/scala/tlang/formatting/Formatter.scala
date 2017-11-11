package tlang.formatting


import better.files.File
import tlang.Constants
import tlang.compiler.lexer.Lexer
import tlang.formatting.grid.Grid
import tlang.formatting.textformatters._
import tlang.messages.{ErrorStringContext, VoidReporter}
import tlang.utils.Extensions._
import tlang.utils.{LogLevel, Logger, LoggingSettings}

object Formatter {
  def apply(formatting: Formatting): Formatter = {
    // The lexer doesn't really need an ErrorStringContext until error messages are generated
    // and the syntax highlighter doesn't generate any errors so we can use null here
    val errorStringContext = ErrorStringContext(null)

    // This lexer should not do any logging
    val lexer = new Lexer(VoidReporter(), errorStringContext) {
      override lazy val logger: Logger = new Logger()(LoggingSettings(logLevel = LogLevel.Off))
    }

    apply(
      formatting,
      WordWrapper(wrapAnsiColors = formatting.useColor),
      Truncator(),
      SyntaxHighlighter(lexer, formatting),
      StackTraceHighlighter(formatting)
    )
  }
}

case class Formatter(
  formatting: Formatting,
  private val wordWrapper: WordWrapper,
  private val truncator: Truncator,
  private val syntaxHighlighter: SyntaxHighlighter,
  private val stackTraceHighlighter: StackTraceHighlighter
) {

  import formatting._

  def grid: Grid = Grid(this)

  def useColor: Boolean = formatting.useColor

  def wrap(text: String, width: Int): List[String] = wordWrapper(text, width)
  def splitWithColors(str: String): List[String] = {
    val lines = str.split("\r?\n", -1).toList
    wordWrapper.wrapAnsiFormatting(lines)
  }

  def truncate(line: String, width: Int): String = truncator(line, width)

  def syntaxHighlight(code: String): String = syntaxHighlighter(code)
  def syntaxHighlight(code: String, marking: Marking): String = syntaxHighlighter(code, marking)
  def syntaxHighlight(code: String, markings: Seq[Marking]): String = syntaxHighlighter(code, markings)

  def highlightStackTrace(throwable: Throwable): String = stackTraceHighlighter(throwable)
  def highlightStackTrace(stackTrace: String): String = stackTraceHighlighter(stackTrace)

  def fileName(file: File): String = fileName(file.nameWithoutExtension)

  def fileName(name: String): String = {
    val color = Bold + Magenta
    color(name + Constants.FileEnding)
  }

  def list(items: String*): String = list(items)
  def list(items: Traversable[String]): String =
    items
      .map(item => s"  ${ Bold(ListMarker) } $item")
      .mkString(NL)


}
