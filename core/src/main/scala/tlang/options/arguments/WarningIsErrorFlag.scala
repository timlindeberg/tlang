package tlang.options.arguments

import tlang.formatting.Formatter
import tlang.options.BooleanFlag

case object WarningIsErrorFlag extends BooleanFlag {
  override val name = "werror"

  override def description(formatter: Formatter): String = {
    "Treats warnings as errors and exits compilation."
  }
}