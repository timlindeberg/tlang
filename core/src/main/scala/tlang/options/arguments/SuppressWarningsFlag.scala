package tlang.options.arguments

import tlang.formatting.Formatter
import tlang.options.BooleanFlag

case object SuppressWarningsFlag extends BooleanFlag {
  override val name = "nowarn"

  override def description(formatter: Formatter): String = {
    "Suppresses warning messages."
  }

}