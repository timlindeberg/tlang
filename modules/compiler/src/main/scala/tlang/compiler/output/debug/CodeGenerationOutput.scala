package tlang
package compiler
package output
package debug

import cafebabe.CodegenerationStackTrace
import tlang.formatting.Formatter
import tlang.formatting.grid.Alignment.Center
import tlang.utils.JSON.Json

case class CodeGenerationOutput(phaseName: String, stackTraces: List[CodegenerationStackTrace])
  (implicit formatter: Formatter) extends Output {
  override def pretty: String = {
    import formatter._

    val grid = formatter.grid.header(Bold("Output after ") + Blue(phaseName.capitalize) + Bold(" phase"))
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
