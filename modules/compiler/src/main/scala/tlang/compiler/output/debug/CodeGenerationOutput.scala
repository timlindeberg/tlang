package tlang.compiler.output.debug

import cafebabe.CodegenerationStackTrace
import tlang.compiler.output.Output
import tlang.formatting.Formatter
import tlang.formatting.grid.Alignment.Center
import tlang.utils.JSON.Json

case class CodeGenerationOutput(phaseName: String, stackTraces: List[CodegenerationStackTrace]) extends Output {
  override def pretty(formatter: Formatter): String = {
    import formatter.formatting._

    val grid = formatter.grid.header(Bold("Output after ") + Blue(phaseName.capitalize))
    stackTraces.foreach { stackTrace =>
      grid
        .row(alignment = Center)
        .content(stackTrace.header)
        .row(5)
        .columnHeaders("Line", "PC", "Height", "ByteCode", "Info")
        .contents(stackTrace.content)
    }
    grid.render()
  }

  override def json: Json = Json()
}
