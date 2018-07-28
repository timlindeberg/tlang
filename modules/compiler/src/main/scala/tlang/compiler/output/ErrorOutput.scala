package tlang.compiler.output
import tlang.formatting.Formatter
import tlang.utils.JSON.Json

case class ErrorOutput(errorMessage: String)(implicit formatter: Formatter) extends Output {
  override def pretty: String = {
    import formatter._
    formatter
      .grid
      .header(Red("Error"))
      .row()
      .content(errorMessage)
      .render()
  }
  override def json: Json = Json("error" -> errorMessage)
}
