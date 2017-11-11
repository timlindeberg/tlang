package tlang.options.arguments

import tlang.formatting.Formatter
import tlang.messages.ErrorStringContext
import tlang.options.ArgumentFlag
import tlang.utils.LogLevel
import tlang.utils.Extensions._

case object LogLevelFlag extends ArgumentFlag[LogLevel] {
  override val name           = "loglevel"
  override val argDescription = "level"

  override def description(formatter: Formatter): String = {
    import formatter.formatting._
    s"Specify log level to use when running the compiler. Default is ${Blue("Off")}."
  }

  override def extendedDescription(formatter: Formatter): String = {
    import formatter.formatting._
    val logLevels = LogLevel.map(Blue(_))
    s"""|Specify log level to use when running the compiler. Default is ${Blue("Off")}.
        |
        |Valid levels are:
        |${formatter.list(logLevels)}
     """.stripMargin
  }

  override def parseValue(args: Set[String]): LogLevel = {
    LogLevel.find { _.getClass.getSimpleName.toLowerCase in args }.get
  }

  protected override def verifyArgument(logLevel: String)(implicit errorContext: ErrorStringContext): Unit = {
    import errorContext.ErrorStringContext
    val logLevels = LogLevel.map { _.getClass.getSimpleName.toLowerCase }
    if (logLevel notIn logLevels)
      error(err"Invalid log level: $logLevel.")
  }

}
