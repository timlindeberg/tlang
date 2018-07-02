package tlang.formatting


import java.nio.file.Path

import better.files.File
import tlang.Constants
import tlang.formatting.grid.Grid
import tlang.formatting.textformatters._
import tlang.utils.Extensions._

object Formatter {

  val SimpleFormatter = Formatter(SimpleFormatting)

  def apply(formatting: Formatting, syntaxHighlighter: SyntaxHighlighter): Formatter = {
    Formatter(
      formatting,
      WordWrapper(wrapAnsiColors = formatting.useColor),
      Truncator(),
      syntaxHighlighter,
      StackTraceHighlighter(formatting, failOnError = true)
    )
  }

  def apply(formatting: Formatting): Formatter = {
    Formatter(
      formatting,
      WordWrapper(wrapAnsiColors = formatting.useColor),
      Truncator(),
      SyntaxHighlighter(formatting)(_ => Nil),
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

  def relativePath(file: File): String = {
    val fileName = file.name
    relativize(file.parent.path) + file.fileSystem.getSeparator + fileName
  }

  private def relativize(path: Path): Path = {
    val absolute = path.toAbsolutePath
    if (absolute.startsWith(Constants.Pwd))
      Constants.Pwd.relativize(absolute)
    else
      absolute
  }

  def list(items: String*): String = list(items)
  def list(items: Traversable[String]): String =
    items
      .map(item => s"  ${ Bold(ListMarker) } $item")
      .mkString(NL)


}
