package tlang
package compiler
package output
import tlang.formatting.Formatter

import tlang.utils.JSON.Json

case class InterruptedOutput()(implicit formatter: Formatter) extends Output {
  override def pretty: String = {
    import formatter._
    val color = Bold + Red
    NL +  formatter.grid.header(color("Compilation interrupted.")).render()
  }
  override def json: Json = Json("interrupted" -> true)
}
