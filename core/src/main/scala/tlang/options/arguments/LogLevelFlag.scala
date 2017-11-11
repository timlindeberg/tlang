package tlang.options.arguments

import tlang.formatting.Formatter
import tlang.messages.ErrorStringContext
import tlang.options.ArgumentFlag
import tlang.utils.Extensions._
import tlang.utils.LogLevel

case object LogLevelFlag extends ArgumentFlag[LogLevel] {
  override val name           = "loglevel"
  override val argDescription = "level"

  override def description(formatter: Formatter): String = {
    import formatter.formatting._
    s"Specifies the log level to use. Default is ${ Blue("Off") }."
  }

  override def extendedDescription(formatter: Formatter): String = {
    import formatter.formatting._
    val logLevels = LogLevel.map(Blue(_))
    s"""|Specifies the log level to use. Default is ${ Blue("Off") }.
        |
        |Valid levels are:
        |${ formatter.list(logLevels) }""".stripMargin
  }

  override def parseValue(args: Set[String]): LogLevel = {
    val argsLower = args.map(_.toLowerCase)
    LogLevel.find { _.name in argsLower } getOrElse LogLevel.Off
  }

  protected override def verify(logLevel: String)(implicit errorContext: ErrorStringContext): Unit = {
    import errorContext.ErrorStringContext
    val logLevels = LogLevel.map { _.getClass.simpleObjectName.toLowerCase }
    if (logLevel.toLowerCase notIn logLevels)
      error(err"Invalid log level: $logLevel.")
  }

}
