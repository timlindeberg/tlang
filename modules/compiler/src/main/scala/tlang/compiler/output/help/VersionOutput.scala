package tlang.compiler.output.help

import tlang.Constants.VersionNumber
import tlang.compiler.output.Output
import tlang.formatting.Formatter
import tlang.utils.JSON.Json

case class VersionOutput() extends Output {
  override def pretty(formatter: Formatter): String = s"T-Compiler $VersionNumber"

  override def json: Json = Json("version" -> VersionNumber)
}
