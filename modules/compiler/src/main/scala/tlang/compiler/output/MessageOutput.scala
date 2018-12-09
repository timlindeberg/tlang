package tlang
package compiler
package output

import tlang.formatting.Formatter
import tlang.utils.JSON.Json

case class MessageOutput(message: String)(implicit formatter: Formatter) extends Output {
  override def pretty: String = formatter.grid.header(message).render()
  override def json: Json = Json()
}
