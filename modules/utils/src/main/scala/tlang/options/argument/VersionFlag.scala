package tlang.options.argument

import tlang.formatting.Formatter
import tlang.options.BooleanFlag

case object VersionFlag extends BooleanFlag {
  override val name = "version"

  override def description(formatter: Formatter): String = {
    "Prints version information and exits."
  }
}