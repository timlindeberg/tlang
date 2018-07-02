package tlang.compiler.output
import tlang.formatting.Formatter

case class SuccessOutput(success: Boolean) extends Output {
  override def pretty(formatter: Formatter): String = ""
  override def json(): Map[String, Any] = Map("success" -> success)
}
