package tlang.compiler.output
import tlang.formatting.Formatter
import tlang.formatting.grid.Alignment.Center
import tlang.formatting.textformatters.{StackTraceHighlighter, SyntaxHighlighter}
import tlang.utils.{ExecutionResult, Source}
import tlang.utils.Extensions._
import tlang.utils.JSON.Json

case class ExecutionResultOutput(stackTraceHighlighter: StackTraceHighlighter, syntaxHighlighter: SyntaxHighlighter, results: Seq[(Source, ExecutionResult)])(implicit formatter: Formatter) extends Output {
  override def pretty: String = {
    val formatting = formatter.formatting
    import formatting._

    val numPrograms = results.size
    val grid = formatter.grid

    def addOutput(source: Source, output: String): Unit = {
      if (output.isEmpty)
        return

      val highlighted = syntaxHighlighter(output)
      val lines = formatter
        .splitWithColors(highlighted)
        .zipWithIndex
        .map { case (line, i) => (Magenta(i + 1), line) }
      grid
        .row(alignment = Center)
        .content(source.description)
        .row(2)
        .contents(lines)
    }

    def addException(source: Source, exception: Throwable) = {
      val stackTrace = removeCompilerPartOfStacktrace(source.mainName, stackTraceHighlighter(exception))
      grid
        .row(alignment = Center)
        .content(source.errorDescription)
        .row()
        .content(stackTrace)
    }


    if(numPrograms == 0)
      return grid.header(s"Execution ${ Red("failed") }, none of the given files contains a main method.").render()

    grid.header(Bold(if (numPrograms > 1) "Executing programs" else "Executing program"))

    results foreach { case (source, result) =>
      addOutput(source, result.output)
      result.exception ifDefined { addException(source, _) }
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

  override def json: Json = Json(
    "execution" -> results.map { case (source, result) =>
      Json(
        "source" -> source.description,
        "output" -> result.output,
        "exception" -> result.exception.map(_.stackTrace)
      )
    }
  )
}
