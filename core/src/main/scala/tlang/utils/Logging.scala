package tlang.utils

import java.text.{DateFormat, SimpleDateFormat}
import java.util.Date

import better.files.File
import sourcecode.{Enclosing, Line, File => SourceFile}
import tlang.formatting.Colors.Color
import tlang.formatting._
import tlang.utils.Extensions._
import tlang.utils.Logging._


sealed abstract class LogLevel(val value: Int) extends Ordered[LogLevel] {

  override def compare(that: LogLevel): Int = value - that.value
  def name: String = getClass.simpleObjectName.toLowerCase

}

object LogLevel extends Enumerable[LogLevel] {
  case object Trace extends LogLevel(0)
  case object Debug extends LogLevel(1)
  case object Info extends LogLevel(2)
  case object Warn extends LogLevel(3)
  case object Error extends LogLevel(4)
  case object Off extends LogLevel(5)

  override lazy val Values: List[LogLevel] = Enumeration.instancesOf[LogLevel].sortBy(_.value)
}


case class LoggingSettings(
  var timeFormat: DateFormat = new SimpleDateFormat("HH:mm:ss:SSS"),
  var threadWidth: Int = 15,
  var locationWidth: Int = 45,
  var logLevelWidth: Int = 5,
  var logThreads: Boolean = false,
  var printToStdout: Boolean = true,
  var formatter: Formatter = Formatter(DefaultFormatting),
  var logLevel: LogLevel = LogLevel.Debug
) {

  def formatting: Formatting = formatter.formatting
  def useColor: Boolean = formatting.useColor

  def TimeColor: Color = formatting.Magenta
  def HighlightColor: Color = formatting.Cyan

  var _printToFile: List[File] = Nil
  def printToFile: List[File] = _printToFile
  def printToFile_=(files: List[File]): Unit = {
    files.foreach(_.createDirectories())
    _printToFile = files
  }
}

object Logging {

  implicit val DefaultLogSettings: LoggingSettings = LoggingSettings(logLevel = sys.env
    .get("logLevel")
    .flatMap(level => LogLevel.find(_.name == level.toLowerCase))
    .getOrElse(LogLevel.Off)
  )

}


trait Logging {

  lazy val logger: Logger = new Logger

  implicit class LoggingStringContext(val sc: StringContext) {

    def trace(args: Any*)(implicit line: Line, file: SourceFile, enclosing: Enclosing): Unit =
      logger.logWithContext(LogLevel.Trace, sc, args)

    def debug(args: Any*)(implicit line: Line, file: SourceFile, enclosing: Enclosing): Unit =
      logger.logWithContext(LogLevel.Debug, sc, args)

    def info(args: Any*)(implicit line: Line, file: SourceFile, enclosing: Enclosing): Unit =
      logger.logWithContext(LogLevel.Info, sc, args)

    def warn(args: Any*)(implicit line: Line, file: SourceFile, enclosing: Enclosing): Unit =
      logger.logWithContext(LogLevel.Warn, sc, args)

    def error(args: Any*)(implicit line: Line, file: SourceFile, enclosing: Enclosing): Unit =
      logger.logWithContext(LogLevel.Error, sc, args)

  }


}

class Logger(implicit protected val loggingSettings: LoggingSettings = DefaultLogSettings) {

  import loggingSettings._

  def logWithContext(logLevel: LogLevel, sc: StringContext, values: Seq[Any])(implicit line: Line, file: SourceFile, enclosing: Enclosing): Unit = {
    if (!isEnabled(logLevel))
      return

    val sb = new StringBuilder
    val expressions = values.iterator
    val strings = sc.parts.iterator
    var last: Any = null
    var nextString: Option[String] = Some(strings.next)

    while (nextString.isDefined) {
      sb ++= formatting.Bold(nextString.get)
      nextString = if (strings.hasNext) Some(strings.next()) else None
      if (expressions.hasNext) {
        val expression = expressions.next
        if (!expressions.hasNext && nextString.exists(_.isEmpty))
          last = expression
        else
          sb ++= highlight(expression)
      }
    }
    val msg = sb.toString()
    putLogStatement(logLevel, msg, last)
  }

  def trace(msg: String, extra: Any = null)(implicit line: Line, file: SourceFile, enclosing: Enclosing): Unit =
    putLogStatement(LogLevel.Trace, msg, extra)
  def debug(msg: String, extra: Any = null)(implicit line: Line, file: SourceFile, enclosing: Enclosing): Unit =
    putLogStatement(LogLevel.Debug, msg, extra)
  def info(msg: String, extra: Any = null)(implicit line: Line, file: SourceFile, enclosing: Enclosing): Unit =
    putLogStatement(LogLevel.Info, msg, extra)
  def warn(msg: String, extra: Any = null)(implicit line: Line, file: SourceFile, enclosing: Enclosing): Unit =
    putLogStatement(LogLevel.Warn, msg, extra)
  def error(msg: String, extra: Any = null)(implicit line: Line, file: SourceFile, enclosing: Enclosing): Unit =
    putLogStatement(LogLevel.Error, msg, extra)

  def isEnabled(logLevel: LogLevel): Boolean = logLevel >= loggingSettings.logLevel

  protected def putLogStatement(level: LogLevel, message: String, extra: Any)(implicit file: SourceFile, line: Line, enclosing: Enclosing): Unit = {
    if (!isEnabled(level))
      return

    val logLevelFormatted = logLevel(level)
    val timestampFormatted = timestamp
    val threadFormatted = thread
    val locationFormatted = location(enclosing.value, file.value, line.value)
    val extraInfoFormatted = extraInfo(level, Option(extra))

    val msg = s"$logLevelFormatted $timestampFormatted$threadFormatted $locationFormatted $message$extraInfoFormatted"

    if (printToStdout)
      println(msg)

    printToFile.foreach(_.appendLine(msg))
  }

  protected def now: Date = new Date()
  protected def threadName: String = Thread.currentThread().getName
  protected def threadId: Int = Thread.currentThread().getId.asInstanceOf[Int]
  protected def fileName(file: String): String = File(file).name

  protected def timestamp: String = {
    val time = timeFormat.format(now)
    if (!useColor)
      return time

    time.split("[.,:\\-]").map(TimeColor(_)).mkString(":")
  }

  protected def thread: String = {
    if (!logThreads)
      return ""

    var name = fit(threadName, threadWidth)

    if (useColor) {
      val allColors = formatting.AllColors
      val color = allColors(threadId % allColors.length)
      name = color(name)
    }

    " [" + name + "]"
  }

  protected def logLevel(level: LogLevel): String = {
    val name = fit(level.getClass.getSimpleName.stripSuffix("$").toUpperCase, logLevelWidth)
    val color = logLevelColor(level)
    color(s"$name " + formatting.Vertical)
  }

  protected def location(enclosing: String, file: String, lineNumber: Int): String = {
    fit(shortenLocation(enclosing, file, lineNumber), locationWidth)
  }

  protected def extraInfo(logLevel: LogLevel, extra: Option[Any]): String = {
    val f = formatting
    import f._

    if (extra.isEmpty)
      return ""

    val color = logLevelColor(logLevel)

    val lines = extraLines(extra.get)
    if (lines.length == 1)
      return lines.head

    val indent = color(Vertical + " ")
    NL + color(TopLeft + Horizontal * logLevelWidth + BottomRight) +
      NL + lines.map(indent + _).mkString(NL) +
      NL + color(BottomLeft + Horizontal * logLevelWidth + TopRight)
  }

  private def extraLines(extra: Any): Seq[String] = extra match {
    case e: Throwable => formatter.highlightStackTrace(e).split("\r?\n", -1)
    case other        =>
      val lines = other.toString.split("\r?\n", -1)
      if (lines.length == 1)
        lines.map(highlight(_))
      else
        lines.map(formatter.syntaxHighlight(_))
  }

  private def fit(s: String, width: Int): String = {
    val len = s.length
    val name = if (len > width)
      "..." + s.drop(3 + (len - width))
    else
      s

    name + " " * (width - name.length)
  }

  private def logLevelColor(logLevel: LogLevel) = logLevel match {
    case LogLevel.Trace => formatting.Blue
    case LogLevel.Debug => formatting.Blue
    case LogLevel.Info  => formatting.Green
    case LogLevel.Warn  => formatting.Yellow
    case LogLevel.Error => formatting.Red
    case LogLevel.Off   => ???
  }


  private def shortenLocation(enclosing: String, file: String, lineNumber: Int): String = {
    val filePos = s"(${ fileName(file) }:$lineNumber)"

    val enc = enclosing.split("[\\.#]")
    var len = enc.map(_.length).sum + enc.length - 1 + filePos.length

    if (len < locationWidth)
      return enclosing + filePos

    // Try truncating one prefix at a time, e.g com -> c, starting from the left
    var i = 0
    while (len > locationWidth && i < enc.length) {
      len -= (enc(i).length - 1)
      i += 1
    }

    if (len <= locationWidth)
      return (enc.take(i).map(_.charAt(0)) ++ enc.drop(i)).mkString(".") + filePos

    // Still doesnt fit, start removing prefixes
    i = 0
    while (len > locationWidth && i < enc.length) {
      len -= 2
      i += 1
    }

    if (len <= locationWidth)
      return enc.drop(i).map(_.charAt(0)).mkString(".") + filePos

    // Still didn't fit, truncate the rest
    enc.map(_.charAt(0)).mkString(".") + filePos
  }

  private def highlight(s: Any) = if (useColor) HighlightColor(s) else s"'$s'"
}
