package tlang.compiler.output.help

import tlang.compiler.output.Output
import tlang.formatting.Formatter
import tlang.options.FlagArgument

case class FlagInfoOutput(flag: FlagArgument[_]) extends Output {

  override def pretty(formatter: Formatter): String = {
    formatter
      .grid
      .header(flag.flagName(formatter.formatting))
      .row()
      .content(flag.extendedDescription(formatter))
      .render()
  }

  override def json(): Map[String, Any] = Map("flag" -> flag.json)
}
