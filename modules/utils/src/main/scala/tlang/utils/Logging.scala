package tlang
package utils

import java.text.SimpleDateFormat
import java.util.Date

import better.files.File
import sourcecode.{Enclosing, Line, File => SourceFile}
import tlang.formatting.Colors.Color
import tlang.formatting._
import tlang.formatting.textformatters.StackTraceHighlighter

import scala.language.implicitConversions

sealed abstract class LogLevel(val priority: Int, private val _color: Color) extends Ordered[LogLevel] {

  override def compare(that: LogLevel): Int = priority - that.priority
  def name: String = getClass.simpleObjectName.toLowerCase
  def color(implicit formatter: Formatter): Color = formatter.translate(_color)
}

object LogLevel extends Enumerable[LogLevel] {
  case object Trace extends LogLevel(0, Colors.Cyan)
  case object Debug extends LogLevel(1, Colors.Blue)
  case object Info extends LogLevel(2, Colors.Green)
  case object Warn extends LogLevel(3, Colors.Yellow)
  case object Error extends LogLevel(4, Colors.Red)
  case object Off extends LogLevel(5, Colors.NoColor)

  override lazy val Values: List[LogLevel] = Enumeration.instancesOf[LogLevel].sortBy(_.priority)
}

case class LoggingSettings(
  var timeFormat: Date => String = (date) => new SimpleDateFormat("HH:mm:ss:SSS").format(date), // (date) => "",
  var threadWidth: Int = 10,
  var locationWidth: Int = 45,
  var logLevelWidth: Int = 5,
  var logThreads: Boolean = false,
  var printToStdout: Boolean = true,
  var formatter: Formatter = Formatter.PrettyFormatter,
  var logLevel: LogLevel = LogLevel.Debug
) {

  def TimeColor: Color = formatter.Magenta
  def HighlightColor: Color = formatter.Cyan

  private var _printToFile: List[File] = Nil
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

class LazyVal(lazyValue: => Any) {
  lazy val value: Any = lazyValue
}

trait Logging {

  lazy val logger: Logger = new Logger

  implicit def AnyToLazy(any: => Any): LazyVal = new LazyVal(any)

  implicit class LoggingStringContext(val sc: StringContext) {
    def trace(args: LazyVal*)(implicit line: Line, file: SourceFile, enclosing: Enclosing): Unit = log(LogLevel.Trace, args)
    def debug(args: LazyVal*)(implicit line: Line, file: SourceFile, enclosing: Enclosing): Unit = log(LogLevel.Debug, args)
    def info(args: LazyVal*)(implicit line: Line, file: SourceFile, enclosing: Enclosing): Unit = log(LogLevel.Info, args)
    def warn(args: LazyVal*)(implicit line: Line, file: SourceFile, enclosing: Enclosing): Unit = log(LogLevel.Warn, args)
    def error(args: LazyVal*)(implicit line: Line, file: SourceFile, enclosing: Enclosing): Unit = log(LogLevel.Error, args)

    private def log(logLevel: LogLevel, values: Seq[LazyVal])(implicit line: Line, file: SourceFile, enclosing: Enclosing): Unit = {
      if (logger.isEnabled(logLevel))
        logger.logWithContext(logLevel, sc, values.map(_.value))
    }
  }
}

class Logger(implicit protected val loggingSettings: LoggingSettings = Logging.DefaultLogSettings) {

  import loggingSettings._

  private implicit val formatter: Formatter = loggingSettings.formatter
  private val stackTraceHighlighter = StackTraceHighlighter(failOnError = false)

  def logWithContext(logLevel: LogLevel, sc: StringContext, values: Seq[Any])(implicit line: Line, file: SourceFile, enclosing: Enclosing): Unit = {
    val sb = new StringBuilder
    val expressions = values.iterator
    val strings = sc.parts.iterator
    var last: Any = null
    var nextString: Option[String] = Some(strings.next)

    while (nextString.isDefined) {
      sb ++= formatter.Bold(nextString.get)
      nextString = if (strings.hasNext) Some(strings.next()) else None
      if (expressions.hasNext) {
        val expression = expressions.next
        if (!expressions.hasNext && nextString.exists(_.isEmpty))
          last = expression
        else
          sb ++= formatValue(expression)
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
    val threadFormatted = thread
    val timestampFormatted = timestamp
    val locationFormatted = location(enclosing.value, file.value, line.value)
    val extraInfoFormatted = extraInfo(level, Option(extra))

    val msg = List(logLevelFormatted, threadFormatted, timestampFormatted, locationFormatted, message)
      .filter(!_.isEmpty)
      .mkString(" ") + extraInfoFormatted

    if (printToStdout)
      println(msg)

    printToFile.foreach(_.appendLine(msg))
  }

  protected def now: Date = new Date()
  protected def threadName: String = Thread.currentThread().getName
  protected def threadId: Int = Thread.currentThread().getId.asInstanceOf[Int]
  protected def fileName(file: String): String = File(file).name

  protected def timestamp: String = {
    val time = timeFormat(now)
    if (!formatter.useColor)
      return time

    time.split("[.,:\\-]").map(TimeColor(_)).mkString(":")
  }

  protected def thread: String = {
    if (!logThreads)
      return ""

    val s = threadName
    var name = s.substring(math.max(s.length - threadWidth, 0), s.length)
    name = fit(name, threadWidth)

    if (formatter.useColor) {
      val allColors = formatter.AllColors
      val color = allColors(threadId % allColors.length)
      name = color(name)
    }

    s"[$name]"
  }

  protected def logLevel(level: LogLevel): String = {
    val name = fit(level.getClass.getSimpleName.stripSuffix("$").toUpperCase, logLevelWidth)
    val color = level.color
    color(s"$name " + formatter.Vertical)
  }

  protected def location(enclosing: String, file: String, lineNumber: Int): String = {
    fit(shortenLocation(enclosing, file, lineNumber), locationWidth)
  }

  protected def extraInfo(logLevel: LogLevel, extra: Option[Any]): String = {
    val f = formatter
    import f._

    if (extra.isEmpty)
      return ""

    val color = logLevel.color

    val lines = extraLines(extra.get)
    if (lines.lengthCompare(1) == 0 || lines.forall(_.isEmpty))
      return lines.head

    val indent = color(Vertical + " ")
    NL + color(TopLeft + Horizontal * logLevelWidth + BottomRight) +
      NL + lines.filter(_.nonEmpty).map(indent + _).mkString(NL) +
      NL + color(BottomLeft + Horizontal * logLevelWidth + TopRight)
  }

  private def extraLines(extra: Any): Seq[String] = extra match {
    case e: Throwable => stackTraceHighlighter(e).split("\r?\n", -1)
    case other        =>
      val lines = other.toString.split("\r?\n", -1)
      if (lines.length == 1) Seq(formatValue(other)) else lines
  }

  private def fit(s: String, width: Int): String = {
    val len = s.length
    val name = if (len > width)
      "..." + s.drop(3 + (len - width))
    else
      s

    name + " " * (width - name.length)
  }

  private def shortenLocation(enclosing: String, file: String, lineNumber: Int): String = {
    val filePos = s"(${ fileName(file) }:$lineNumber)"

    var enc = enclosing.split("[\\.#]")
    // Remove everything after the original method name like $anonFunc etc.
    enc = enc.updated(enc.length - 1, enc.last.split(" ").head)
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

  private def formatValue(value: Any): String = {
    val s = value match {
      case f: File => f.path.relativePWD
      case _       => value.toString
    }
    if (formatter.useColor) HighlightColor(s) else s"'$s'"
  }
}
