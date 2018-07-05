package tlang.compiler.output
import tlang.formatting.Formatter
import tlang.utils.JSON.Json

case class MessageOutput(message: String) extends Output {
  override def pretty(formatter: Formatter): String = {
    formatter.grid.header(message).render()
  }
  override def json: Json = Json()
}
