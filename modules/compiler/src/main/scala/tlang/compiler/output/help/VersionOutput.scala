package tlang.compiler.output.help

import tlang.Constants.VersionNumber
import tlang.compiler.output.Output
import tlang.formatting.Formatter
import tlang.utils.JSON.Json

case class VersionOutput(formatter: Formatter) extends Output {
  override def pretty: String = {
    import formatter.formatting._
    formatter.grid.header(s"T-Compiler ${Green(VersionNumber)}").render()
  }

  override def json: Json = Json("version" -> VersionNumber)
}
