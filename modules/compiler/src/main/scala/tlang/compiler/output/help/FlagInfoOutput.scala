package tlang
package compiler
package output
package help

import tlang.formatting.Formatter
import tlang.options.FlagArgument
import tlang.utils.JSON.Json

case class FlagInfoOutput(flag: FlagArgument[_])(implicit formatter: Formatter) extends Output {

  override def pretty: String = {
    formatter
      .grid
      .header(flag.flagName)
      .row()
      .content(flag.getExtendedDescription)
      .render()
  }

  override def json: Json = Json("flag" -> flag.json)
}
