package tlang.formatting


import java.nio.file.Path

import better.files.File
import tlang.Constants
import tlang.formatting.grid.Grid
import tlang.formatting.textformatters._
import tlang.utils.Extensions._

object Formatter {

  val SimpleFormatter = Formatter(SimpleFormatting)
  val PrettyFormatter = Formatter(PrettyFormatting)

  def apply(formatting: Formatting): Formatter = {
    Formatter(formatting, WordWrapper(wrapAnsiColors = formatting.useColor), Truncator(), TabReplacer(2))
  }
}

case class Formatter(
  formatting: Formatting,
  wordWrap: WordWrapper,
  truncate: Truncator,
  replaceTabs: TabReplacer
) {

  import formatting._

  def grid: Grid = Grid()(this)

  def useColor: Boolean = formatting.useColor

  def splitWithColors(str: String): List[String] = {
    val lines = str.split("\r?\n", -1).toList
    wordWrap.wrapAnsiFormatting(lines)
  }

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
