package tlang.compiler.output.help

import tlang.compiler.output.Output
import tlang.formatting.{Formatter, Formatting}
import tlang.options.FlagArgument
import tlang.utils.JSON.Json

case class FlagInfoOutput(formatter: Formatter, flag: FlagArgument[_]) extends Output {

  private implicit val f: Formatter = formatter

  override def pretty: String = {
    formatter
      .grid
      .header(flag.flagName)
      .row()
      .content(flag.extendedDescription)
      .render()
  }

  override def json: Json = Json("flag" -> flag.json)
}
