package tlang
package options
package argument

import tlang.formatting.{ErrorStringContext, Formatter}

import scala.tools.jline.TerminalFactory

case object LineWidthFlag extends NumberFlag {
  override val defaultValue: Int = -1

  val DefaultWidth = 80

  override val name = "linewidth"

  override def description(implicit formatter: Formatter): String =
    s"""
       |Specifies the width of a line in error messages and output.
       |If none is given the width of the terminal will be used, if it can be determined. Otherwise, ${ highlight(DefaultWidth) } will be used.
      """

  protected override def verify(arg: String)(implicit errorContext: ErrorStringContext): Unit = {
    super.verify(arg)
    import errorContext.ErrorStringContext
    val num = arg.toInt
    if (num < -1) {
      error(err"$num is not a valid line length.")
    }
  }

  override def parseValue(args: Set[String]): Int = {
    val givenWidth = super.parseValue(args)
    if (givenWidth != -1)
      givenWidth
    else if (System.console() == null)
      DefaultWidth
    else
      TerminalFactory.create().getWidth
  }
}
