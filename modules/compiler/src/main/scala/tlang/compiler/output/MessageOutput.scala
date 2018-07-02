package tlang.compiler.output
import tlang.formatting.Formatter

case class MessageOutput(message: String) extends Output {
  override def pretty(formatter: Formatter): String = {
    formatter.grid.header(message).render()
  }
  override def json(): Map[String, Any] = Map()
}
