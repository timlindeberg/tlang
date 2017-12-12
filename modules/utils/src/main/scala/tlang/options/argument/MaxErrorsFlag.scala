package tlang.options.argument

import tlang.formatting.Formatter
import tlang.options.NumberFlag

case object MaxErrorsFlag extends NumberFlag {
  override val defaultValue = 100

  override val name: String = "maxerrors"

  override def description(formatter: Formatter): String = {
    import formatter.formatting._
    s"""
       |Specify the maximum number of errors to report. The default is '${ Blue(defaultValue) }'.
       |Enter a negative number to show all errors.
       """.stripMargin.trim
  }

}