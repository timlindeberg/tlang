package tlang.compiler.output

import cafebabe.CodeFreezingException
import tlang.formatting.Formatter
import tlang.formatting.grid.Alignment.Center
import tlang.formatting.textformatters.StackTraceHighlighter
import tlang.utils.Extensions._
import tlang.utils.JSON.Json

case class InternalErrorOutput(stackTraceHighlighter: StackTraceHighlighter, error: Throwable)(implicit formatter: Formatter) extends Output {
  override def pretty: String = {
    import formatter.formatting._
    val stackTrace = stackTraceHighlighter(error)

    val grid = formatter
      .grid
      .header(s"${ Bold }Compilation ${ Red("failed") }${ Bold(" with an unexpected error") }")
      .row()
      .content(stackTrace)

    // Handling of special errors
    error match {
      case CodeFreezingException(_, Some(stackTrace)) =>
        grid
          .row(alignment = Center)
          .content(Bold(Blue("Stack trace at time of code freezing")))
          .emptyLine()
          .content(stackTrace.header)
          .row(5)
          .columnHeaders("Line", "PC", "Height", "ByteCode", "Info")
          .contents(stackTrace.content)
      case _                                          =>
    }

    grid.render()
  }

  override def json: Json = Json("internalError" -> error.stackTrace)
}
