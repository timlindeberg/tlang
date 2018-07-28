package tlang.compiler.argument

import tlang.formatting.Formatter
import tlang.options.BooleanFlag


case object ExecFlag extends BooleanFlag {
  override val name = "exec"

  override def description(implicit formatter: Formatter): String =
    "Executes all compiled main methods after compilation."

}
