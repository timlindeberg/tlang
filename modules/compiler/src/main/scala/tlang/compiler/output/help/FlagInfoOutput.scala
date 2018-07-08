package tlang.compiler.output.help

import tlang.compiler.output.Output
import tlang.formatting.Formatter
import tlang.options.FlagArgument
import tlang.utils.JSON.Json

case class FlagInfoOutput(formatter: Formatter, flag: FlagArgument[_]) extends Output {

  override def pretty: String = {
    formatter
      .grid
      .header(flag.flagName(formatter.formatting))
      .row()
      .content(flag.extendedDescription(formatter))
      .render()
  }

  override def json: Json = Json("flag" -> flag.json)
}
