package tlang.compiler.argument

import tlang.formatting.Formatter
import tlang.options.BooleanFlag


case object JsonFlag extends BooleanFlag {
  override val name = "json"

  override def description(formatter: Formatter): String = {
    "Outputs error messages and warnings in JSON."
  }

}
