package tlang.options.arguments

import tlang.formatting.Formatter
import tlang.options.BooleanFlag

case object NoColorFlag extends BooleanFlag {
  override val name = "no-color"

  override def description(formatter: Formatter): String = {
    import formatter.formatting._
    s"Removes all $Bold${ Red }c${ Green }o${ Blue }l${ Yellow }o${ Magenta }r${ Reset } from the output."
  }
}