package tlang.options.argument

import tlang.formatting.{ErrorStringContext, Formatter}
import tlang.options.NumberFlag

case object MessageContextFlag extends NumberFlag {
  override val defaultValue = 2

  override val name      = "messagecontext"
  override val shortFlag = Some("c")

  override def description(implicit formatter: Formatter): String =
    s"""
       |Specify how many lines to display around an error position in error messages and warnings.
       |The default is ${ highlight(defaultValue) }.
      """

  protected override def verify(arg: String)(implicit errorContext: ErrorStringContext): Unit = {
    super.verify(arg)
    import errorContext.ErrorStringContext
    val num = arg.toInt
    if (num < -1) {
      error(err"$num is not a number of message context lines.")
    }
  }

}
