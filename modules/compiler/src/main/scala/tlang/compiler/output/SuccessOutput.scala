package tlang.compiler.output
import tlang.formatting.Formatter
import tlang.utils.JSON.Json

case class SuccessOutput(success: Boolean) extends Output {
  override def pretty(formatter: Formatter): String = ""
  override def json: Json = Json("success" -> success)
}
