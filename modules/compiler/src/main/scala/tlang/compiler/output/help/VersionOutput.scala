package tlang.compiler.output.help

import tlang.Constants.VersionNumber
import tlang.compiler.output.Output
import tlang.formatting.Formatter

case class VersionOutput() extends Output {
  override def pretty(formatter: Formatter): String = s"T-Compiler $VersionNumber"

  override def json(): Map[String, Any] = Map("version" -> VersionNumber)
}
