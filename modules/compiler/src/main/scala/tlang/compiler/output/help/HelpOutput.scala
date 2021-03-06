package tlang
package compiler
package output
package help

import tlang.formatting.Formatter
import tlang.formatting.grid.{Column, Width}
import tlang.options.FlagArgument
import tlang.utils.JSON.Json

case class HelpOutput(commandName: String, flagArguments: Set[FlagArgument[_]])(implicit formatter: Formatter) extends Output {

  override def pretty: String = {

    import formatter._

    val tcompile = (Bold + Green) (commandName)
    val options = Blue("options")
    val source = Blue("source files")
    val optionsHeader = Bold(Magenta("Options"))
    val flags = flagArguments
      .toList
      .sortBy { _.name }
      .map { flag => (flag.flagName, flag.getDescription) }

    val maxFlagWidth = flags.map(_._1.visibleCharacters).max

    val grid = formatter
      .grid
      .header(s"> $tcompile <$options> <$source> $NL$NL $optionsHeader")

    flags.foreach { columns =>
      grid.row(Column(width = Width.Fixed(maxFlagWidth)), Column)
      grid.contents(columns)
    }

    grid.render()
  }

  override def json: Json = Json("flags" -> flagArguments.map(_.json))
}
