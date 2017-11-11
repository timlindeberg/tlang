package tlang.options.arguments

import tlang.formatting.Formatter
import tlang.messages.ErrorStringContext
import tlang.options.NumberFlag

import scala.tools.jline.TerminalFactory

case object LineWidthFlag extends NumberFlag {
  override val defaultValue: Int = -1

  val DefaultWidth = 80

  override val name = "linewidth"

  override def description(formatter: Formatter): String = {
    import formatter.formatting._
    s"""
       |Specifies the width of a line in error messages and output.
       |If none is given (or ${ Blue(-1) } is given) the width of the terminal will be used, if it can be determined.
       |Otherwise, '${ Blue(DefaultWidth) }' will be used.
      """.stripMargin.trim
  }

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