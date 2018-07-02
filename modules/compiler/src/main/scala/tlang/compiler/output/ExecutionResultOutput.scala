package tlang.compiler.output
import better.files.File
import tlang.formatting.Formatter
import tlang.formatting.grid.Alignment.Center
import tlang.utils.ExecutionResult
import tlang.utils.Extensions._

case class ExecutionResultOutput(results: Seq[(File, ExecutionResult)]) extends Output {
  override def pretty(formatter: Formatter): String = {
    import formatter.formatting._

    val numPrograms = results.size
    val grid = formatter.grid

    def addOutput(file: File, output: String): Unit = {
      if (output.isEmpty)
        return

      val highlighted = formatter.syntaxHighlight(output)
      val lines = formatter
        .splitWithColors(highlighted)
        .zipWithIndex
        .map { case (line, i) => (Magenta(i + 1), line) }
      grid
        .row(alignment = Center)
        .content(formatter.fileName(file))
        .row(2)
        .contents(lines)
    }

    def addException(fileName: String, exception: Throwable) = {
      import formatter.formatting._
      val errorColor = Red + Bold
      val stackTrace = removeCompilerPartOfStacktrace(fileName, formatter.highlightStackTrace(exception))
      grid
        .row(alignment = Center)
        .content(errorColor(fileName))
        .row()
        .content(stackTrace)
    }


    if(numPrograms == 0)
      return grid.header(s"Execution ${ Red("failed") }, none of the given files contains a main method.").render()

    grid.header(Bold(if (numPrograms > 1) "Executing programs" else "Executing program"))

    results foreach { case (file, result) =>
      addOutput(file, result.output)
      result.exception ifDefined { addException(file.name, _) }
    }

    grid.render()
  }

  private def removeCompilerPartOfStacktrace(fileName: String, stackTrace: String) = {
    val stackTraceLines = stackTrace.lines.toList
    val lastRow = stackTraceLines.lastIndexWhere(_.contains(fileName))
    if (lastRow == -1 || lastRow + 1 >= stackTraceLines.length)
      stackTrace
    else
      stackTraceLines.take(lastRow + 1).mkString(NL)
  }

  override def json(): Map[String, Any] = Map(
    "execution" -> results.map { case (file, result) =>
      Map(
        "file" -> file,
        "output" -> result.output,
        "exception" -> result.exception.map(_.stackTrace)
      )
    }
  )
}
