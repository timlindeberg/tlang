package tlang.compiler.argument

import tlang.formatting.Formatter
import tlang.options.BooleanFlag

case object ReadStdinFlag extends BooleanFlag {
  override val name      = "read-stdin"

  override def description(formatter: Formatter): String =
    "Reads the content to compile from stdin."

}

