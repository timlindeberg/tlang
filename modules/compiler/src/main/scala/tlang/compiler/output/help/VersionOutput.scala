package tlang.compiler.output.help

import tlang.Constants
import tlang.compiler.output.Output
import tlang.formatting.Formatter
import tlang.utils.JSON.Json

case class VersionOutput(formatter: Formatter) extends Output {
  override def pretty: String = {
    import formatter.formatting._
    formatter.grid.header(s"T-Compiler version ${ Green(Constants.Version) }").render()
  }

  override def json: Json = Json("version" -> Constants.Version)
}
