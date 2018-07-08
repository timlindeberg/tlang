package tlang.compiler.output
import tlang.formatting.Formatter
import tlang.utils.JSON.Json

case class ErrorOutput(formatter: Formatter, errorMessage: String) extends Output {
  override def pretty: String = {
    import formatter.formatting._
    formatter
      .grid
      .header(Red("Error"))
      .row()
      .content(errorMessage)
      .render()
  }
  override def json: Json = Json("error" -> errorMessage)
}
