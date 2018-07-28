package tlang.compiler.argument

import tlang.formatting.Formatter
import tlang.options.BooleanFlag

case object ReadStdinFlag extends BooleanFlag {
  override val name      = "readstdin"

  override def description(implicit formatter: Formatter): String =
    "Reads the content to compile from stdin."

}

