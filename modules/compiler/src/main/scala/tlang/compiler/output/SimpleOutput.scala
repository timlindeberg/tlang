package tlang
package compiler
package output

import tlang.formatting.Formatter
import tlang.utils.JSON.Json

case class SimpleOutput(message: String)(implicit formatter: Formatter) extends Output {
  override def pretty: String = formatter.grid.row().content(message).render()
  override def json: Json = Json()
}