package tlang
package options
package argument

import tlang.formatting.{ErrorStringContext, Formatter}
import tlang.options.NumberFlag

case object TabWidthFlag extends NumberFlag {
  override val defaultValue: Int = 4
  override val name = "tabwidth"

  override def description(implicit formatter: Formatter): String =
    s"""
       |Specifies the tab width to use in error messages and other output.
       |The default value is ${highlight(defaultValue)}
      """

  protected override def verify(arg: String)(implicit errorContext: ErrorStringContext): Unit = {
    super.verify(arg)
    import errorContext.ErrorStringContext
    val num = arg.toInt
    if (num < 1) {
      error(err"$num is not a valid tab width. Has to be a value >= 1.")
    }
  }
}
