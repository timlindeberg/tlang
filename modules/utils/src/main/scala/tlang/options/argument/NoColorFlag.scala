package tlang
package options
package argument

import tlang.formatting.Formatter

case object NoColorFlag extends BooleanFlag {
  override val name = "nocolor"

  override def description(implicit formatter: Formatter): String =
    s"Removes all ${ highlight("color") } from the output."
}
