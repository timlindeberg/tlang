package tlang
package compiler
package argument

import tlang.formatting.Formatter
import tlang.options.BooleanFlag

case object VerboseFlag extends BooleanFlag {
  override val name = "verbose"
  override val shortFlag = Some("v")

  override def description(implicit formatter: Formatter): String =
    "Prints additional information during compilation."
}
