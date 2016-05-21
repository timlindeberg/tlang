package tcompiler
package utils

import java.io.File
import java.util.regex.Matcher

import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.util.parsing.combinator.RegexParsers

class CompilationException(message: String) extends Exception(message)

class Reporter(
  suppressWarnings: Boolean = false,
  warningIsError: Boolean = false,
  useColor: Boolean = true,
  maxErrors: Int = 100) {

  private val ErrorSeperator = "\n"
  private def QuoteColor = GetColor(Console.MAGENTA)
  private def NumColor = GetColor(Console.BLUE)
  private def MessageStyle = GetColor(Console.BOLD)
  private def WarningColor = GetColor(Console.YELLOW)
  private def ErrorColor = GetColor(Console.RED + Console.BOLD)
  private def FatalColor = GetColor(Console.RED)
  private def Underline = GetColor(Console.UNDERLINED)
  private def Bold = GetColor(Console.BOLD)
  private def EndColor = GetColor(Console.RESET)

  // Only output color to consoles
  private def GetColor(color: String) = if (useColor) color else ""
  private var filesToLines = Map[File, IndexedSeq[String]]()

  private var hitMaxErrors = false

  var errors   = ArrayBuffer[String]()
  var warnings = ArrayBuffer[String]()


  def warning(errorPrefix: String, errorCode: Int, msg: String, pos: Positioned = NoPosition): Unit = {
    if (warningIsError) {
      error(errorPrefix, errorCode, msg, pos)
      return
    }

    if (suppressWarnings)
      return

    val warning = errMessage(errorPrefix, 1, errorCode, msg, pos)
    warnings += warning
  }


  def error(errorPrefix: String, errorCode: Int, msg: String, pos: Positioned = NoPosition): Unit = {
    if(maxErrors == -1 || errors.size < maxErrors){
      errors += errMessage(errorPrefix, 2, errorCode, msg, pos)
      return
    }

    if(hitMaxErrors)
      return

    val num = s"$ErrorColor$maxErrors$EndColor"
    errors.insert(0, s"There were more than $num errors, only showing the first $num:")
    hitMaxErrors = true
}


  def fatal(errorPrefix: String, errorCode: Int, msg: String, pos: Positioned = NoPosition): Nothing = {
    val error = errMessage(errorPrefix, 3, errorCode, msg, pos)
    errors += error
    throw new CompilationException(error)
  }

  def clear() = {
    errors.clear()
    warnings.clear()
    hitMaxErrors = false
  }

  def hasErrors = errors.nonEmpty
  def hasWarnings = warnings.nonEmpty

  def errorsString = errors.mkString(ErrorSeperator)
  def warningsString = warnings.mkString(ErrorSeperator)

  def terminateIfErrors() =
    if (hasErrors)
      throw new CompilationException(errorsString)


  private def errMessage(locationPrefix: String, errorLevel: Int, errorCode: Int, msg: Any, pos: Positioned) = {
    val prefix = constructErrorPrefix(locationPrefix, errorLevel, errorCode)
    val msgStr = handleQuoteLiterals(msg.toString)
    var sb = new StringBuilder
    if (pos.hasPosition)
      sb ++= filePrefix(pos) + "\n"

    sb ++= s"$prefix: $msgStr"

    if (pos.hasPosition)
      sb ++= locationIndicator(errorLevel, pos)

    sb.toString()
  }

  private def constructErrorPrefix(errorPrefix: String, errorLevel: Int, errorCode: Int) = {
    val code = leftPad(errorCode)

    errorLevel match {
      case 1 => s"${WarningColor}Warning$EndColor ($WarningColor${errorPrefix}1$code$EndColor)"
      case 2 => s"${ErrorColor}Error$EndColor ($ErrorColor${errorPrefix}2$code$EndColor)"
      case 3 => s"${FatalColor}Fatal$EndColor ($FatalColor${errorPrefix}3$code$EndColor)"
      case _ => ???
    }
  }

  private def leftPad(num: Int) = num match {
    case x if x >= 0 && x < 10     => "00" + x
    case x if x >= 10 && x < 100   => "0" + x
    case x if x >= 100 && x < 1000 => x
    case _                         => ???
  }

  private def filePrefix(pos: Positioned) = {
    val Style = Bold + NumColor
    var position = pos.position
    if (useColor) {
      val fileName = pos.file.getName.replaceAll(Main.FileEnding, "")
      position = position.replaceAll("(\\d)", s"$Style$$1$EndColor")
      position = position.replaceAll(fileName, s"$Style$fileName$EndColor")
    }

    s"$Bold[$EndColor$position$Bold]$EndColor"
  }

  private def handleQuoteLiterals(msg: String) = {
    val msgFormat = EndColor + MessageStyle

    val re = """'(.+?)'""".r

    val s = re.replaceAllIn(msg, m => {
      val name = TemplateNameParser.parseTemplateName(m.group(1))
      Matcher.quoteReplacement("\'" + QuoteColor + name + msgFormat + "\'") // escape dollar signs etc.
    })
    msgFormat + s + EndColor
  }

  private def locationIndicator(errorLevel: Int, pos: Positioned): String = {
    val lines = getLines(pos.file)
    val numColor = Bold + NumColor
    val prefix = s"$numColor${pos.line}$EndColor:   "

    var sb = new StringBuilder
    sb ++= "\n" ++ prefix


    if (pos.line - 1 >= lines.size)
      return sb.toString() + "<line unavailable in source file>"

    val line = lines(pos.line - 1)
    val firstNonWhiteSpace = line.indexWhere(c => !c.isWhitespace)

    val leftTrimmedLine = line.substring(firstNonWhiteSpace)

    val start = pos.col - firstNonWhiteSpace - 1
    var end = if (pos.endLine == pos.line) {
      pos.endCol - firstNonWhiteSpace - 1
    } else {
      val firstSlash = leftTrimmedLine.indexOf("//")
      if (firstSlash != -1)
        firstSlash - 1
      else
        leftTrimmedLine.length
    }


    // Back cursor so we never underline whitespaces
    while (leftTrimmedLine(end - 1).isWhitespace)
      end -= 1


    if (useColor) {
      val errorStyle = errorLevel match {
        case 1 => WarningColor
        case 2 => ErrorColor
        case 3 => FatalColor
        case _ => ???
      }

      val pre = leftTrimmedLine.substring(0, start)
      val highlighted = leftTrimmedLine.substring(start, end)
      val post = leftTrimmedLine.substring(end, leftTrimmedLine.length)
      sb ++= pre ++ Underline ++ errorStyle ++ highlighted ++ EndColor ++ post
    } else {
      sb ++= leftTrimmedLine ++ "\n" ++ " " * (start + prefix.length) ++ "~" * (end - start)
    }
    sb ++= "\n"
    sb.toString
  }


  private def getLines(f: File): IndexedSeq[String] = {
    filesToLines.get(f) match {
      case Some(lines) =>
        lines

      case None =>
        val source = Source.fromFile(f).withPositioning(true)
        val lines = source.getLines().toIndexedSeq
        source.close()

        filesToLines += f -> lines

        lines
    }
  }
}

object TemplateNameParser extends RegexParsers {

  import tcompiler.modification.Templates._

  def parseTemplateName(s: String): String = {
    val first = s.indexOf('-')
    if (first == -1)
      return s

    val last = s.lastIndexOf('-')
    if (last == -1)
      return s

    val preFix = s.substring(0, first)
    val middle = s.substring(first, last + 1)
    val postFix = s.substring(last + 1, s.length)

    parseAll(template, middle) match {
      case Success(res, _) => preFix + res + postFix
      case Failure(_, _)   => s
      case _               => ???
    }
  }

  // Grammar

  private def word: Parser[String] =
    """[a-zA-Z\d_]+""".r ^^ {
      _.toString
    }

  private def typeList: Parser[String] = ((Seperator ~ (word | template)) +) ^^ {
    case list => list.map(_._2).mkString(", ")
  }

  private def template: Parser[String] = StartEnd ~ word ~ typeList ~ StartEnd ^^ {
    case _ ~ word ~ args ~ _ => s"$word<$args>"
  }
}

