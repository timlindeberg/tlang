package tlang.compiler.output.help

import tlang.compiler.output.Output
import tlang.formatting.Formatter
import tlang.formatting.grid.{Column, Width}
import tlang.options.FlagArgument
import tlang.utils.Extensions._
import tlang.utils.JSON.Json

case class HelpOutput(formatter: Formatter, flagArguments: Set[FlagArgument[_]]) extends Output {
  override def pretty: String = {
    val formatting = formatter.formatting
    import formatter.formatting._

    val tcomp = Green("tcompile")
    val options = Blue("options")
    val source = Blue("source files")
    val optionsHeader = Bold(Magenta("Options"))
    val flags = flagArguments
      .toList
      .sortBy { _.name }
      .map { flag => (flag.flagName(formatting), flag.description(formatter)) }

    val maxFlagWidth = flags.map(_._1.visibleCharacters).max

    val grid = formatter
      .grid
      .header(s"> $tcomp <$options> <$source> \n\n $optionsHeader")

    flags.foreach { columns =>
      grid.row(Column(width = Width.Fixed(maxFlagWidth)), Column)
      grid.contents(columns)
    }

    grid.render()
  }

  override def json: Json = Json("flags" -> flagArguments.map(_.json))
}
