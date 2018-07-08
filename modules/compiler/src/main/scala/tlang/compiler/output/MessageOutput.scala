package tlang.compiler.output
import tlang.formatting.Formatter
import tlang.utils.JSON.Json

case class MessageOutput(formatter: Formatter, message: String) extends Output {
  override def pretty: String = formatter.grid.header(message).render()
  override def json: Json = Json()
}
