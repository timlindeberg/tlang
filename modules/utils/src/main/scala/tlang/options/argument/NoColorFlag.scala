package tlang.options.argument

import tlang.formatting.Formatter
import tlang.options.BooleanFlag

case object NoColorFlag extends BooleanFlag {
  override val name = "nocolor"

  override def description(implicit formatter: Formatter): String =
    s"Removes all ${ highlight("color") } from the output."
}
