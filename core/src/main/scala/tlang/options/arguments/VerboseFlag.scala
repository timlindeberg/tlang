package tlang.options.arguments

import tlang.formatting.Formatter
import tlang.options.BooleanFlag

case object VerboseFlag extends BooleanFlag {
  override val name      = "verbose"
  override val shortFlag = Some("v")

  override def description(formatter: Formatter): String = {
    """
      |Prints additional information during compilation such as elapsed time
      |for each compilation phase.
    """.stripMargin.trim
  }

}
