package tlang.compiler.argument

import tlang.formatting.Formatter
import tlang.options.BooleanFlag


case object ExecFlag extends BooleanFlag {
  override val name = "exec"

  override def description(formatter: Formatter): String = {
    "Executes all main methods after compilation."
  }

}
