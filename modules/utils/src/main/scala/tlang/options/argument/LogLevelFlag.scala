package tlang
package options
package argument

import tlang.formatting.{ErrorStringContext, Formatter}
import tlang.options.ArgumentFlag

import tlang.utils.LogLevel

case object LogLevelFlag extends ArgumentFlag[LogLevel] {
  override val name           = "loglevel"
  override val argDescription = "level"

  override def description(implicit formatter: Formatter): String =
    s"Specifies the log level to use. Default is ${ highlight("Off") }."

  override def extendedDescription(implicit formatter: Formatter): String =
    s"""
       |Specifies the log level to use. Default is ${ highlight("Off") }.
       |
       |Valid levels are:
       |$logLevels
      """

  override def parseValue(args: Set[String]): LogLevel = {
    val argsLower = args.map(_.toLowerCase)
    LogLevel.find { _.name in argsLower } getOrElse LogLevel.Off
  }

  protected override def verify(logLevel: String)(implicit errorContext: ErrorStringContext): Unit = {
    import errorContext.ErrorStringContext
    val logLevels = LogLevel.map { _.name }
    if (logLevel.toLowerCase notIn logLevels)
      error(err"Invalid log level: $logLevel.")
  }

  private def logLevels(implicit formatter: Formatter): String = {
    val logLevels = LogLevel.map { logLevel =>
      val color = logLevel.color
      color(logLevel)
    }
    formatter.list(logLevels)
  }

}
