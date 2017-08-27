package tlang.options.arguments

import tlang.formatting.Formatter
import tlang.options.BooleanFlag

case object PhasesFlag extends BooleanFlag {
  override val name: String = "phases"
  override def description(formatter: Formatter): String =
    "Prints information about the phases of the T-Compiler and exits."
}
