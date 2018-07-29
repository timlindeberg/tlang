package tlang
package options
package argument

import tlang.formatting.Formatter
import tlang.options.BooleanFlag

case object SuppressWarningsFlag extends BooleanFlag {
  override val name = "nowarn"

  override def description(implicit formatter: Formatter): String =
    "Suppresses warning messages."

}
