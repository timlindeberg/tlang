package tlang.compiler.output
import better.files.File
import tlang.formatting.Formatter
import tlang.formatting.grid.{CenteredColumn, EvenlySpaced}

case class FilesToCompileOutput(filesToCompile: Set[File]) extends Output {
  override def pretty(formatter: Formatter): String = {
    import formatter.formatting._

    val numFiles = filesToCompile.size
    val end = if (numFiles > 1) "files" else "file"

    val grid = formatter.grid.header(Bold("Compiling") + " " + Blue(numFiles) + " " + Bold(end))

    val fileNames = filesToCompile.toList.map(formatter.fileName).sorted
    grid
      .row(CenteredColumn)
      .content(EvenlySpaced(fileNames))
      .render()
  }

  override def json(): Map[String, Any] = Map("compiledFiles" -> filesToCompile)
}
