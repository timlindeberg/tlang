package tlang.compiler.argument

import tlang.formatting.Formatter
import tlang.options.BooleanFlag

case object WarningIsErrorFlag extends BooleanFlag {
  override val name = "werror"

  override def description(implicit formatter: Formatter): String =
    "Treats warnings as errors."

}
