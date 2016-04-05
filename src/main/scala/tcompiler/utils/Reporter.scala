package tcompiler
package utils

import java.io.File
import java.util.regex.Matcher

import org.backuity.ansi.AnsiFormatter.FormattedHelper

import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.util.parsing.combinator.RegexParsers

class CompilationException(message: String) extends Exception(message)

class Reporter(suppressWarnings: Boolean = false) {

  var ErrorSeperator = "\n\n"
  var QuoteColor = Console.MAGENTA
  var MessageStyle = Console.BOLD
  var WarningColor = Console.YELLOW
  var ErrorColor = Console.RED
  var FatalColor = Console.RED

  var errors   = ArrayBuffer[String]()
  var warnings = ArrayBuffer[String]()

  private var filesToLines = Map[File, IndexedSeq[String]]()

  def warning(locationPrefix: String, errorCode: Int, msg: Any, pos: Positioned = NoPosition): Unit = {
    if(suppressWarnings)
      return

    val warning = errMessage(locationPrefix, 1, errorCode, msg, pos)
    warnings += warning
  }

  def error(locationPrefix: String, errorCode: Int, msg: String, pos: Positioned = NoPosition): Unit =
    errors += errMessage(locationPrefix, 2, errorCode, msg, pos)


  def fatal(locationPrefix: String, errorCode: Int, msg: String, pos: Positioned = NoPosition): Nothing = {
    val error = errMessage(locationPrefix, 3, errorCode, msg, pos)
    throw new CompilationException(error)
  }

  def clear() = {
    errors.clear()
    warnings.clear()
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
      case 1 => ansi"%yellow{Warning} (%yellow{${errorPrefix}1$code})"
      case 2 => ansi"%red{%bold{Error}} (%red{%bold{${errorPrefix}2$code}})"
      case 3 => ansi"%red{%bold{Fatal}} (%red{%bold{${errorPrefix}3$code}})"
      case _ => ???
    }
  }

  private def leftPad(num: Int) = num match {
    case x if x >= 0 && x < 10     => "00" + x
    case x if x >= 10 && x < 100   => "0" + x
    case x if x >= 100 && x < 1000 => x
    case _                         => ???
  }

  private def filePrefix(pos: Positioned) = ansi"%bold{[}${pos.position}%bold{]}"

  private def handleQuoteLiterals(msg: String) = {
    val msgFormat = Console.RESET + MessageStyle

    val re = """'(.+?)'""".r

    val s = re.replaceAllIn(msg, m => {
      val name = TemplateNameParser.parseTemplateName(m.group(1))
      Matcher.quoteReplacement("\'" + QuoteColor + name + msgFormat + "\'") // escape dollar signs etc.
    })
    msgFormat + s + Console.RESET
  }

  private def locationIndicator(errorLevel: Int, pos: Positioned) = {
    val lines = getLines(pos.file)
    var sb = new StringBuilder
    sb ++= "\n"
    val errorStyle = errorLevel match {
      case 1 => WarningColor
      case 2 => ErrorColor
      case 3 => FatalColor
      case _ => ???
    }
    if (pos.line - 1 < lines.size) {
      val line = lines(pos.line - 1)
      val firstNonWhiteSpace = line.indexWhere(c => !c.isWhitespace)

      val l = line.substring(firstNonWhiteSpace)

      val start = pos.col - 1 - firstNonWhiteSpace
      var end = if (pos.endLine == pos.line) {
        pos.endCol - 1 - firstNonWhiteSpace
      } else {
        val firstSlash = l.indexOf("//")
        if (firstSlash != -1)
          firstSlash - 1
        else
          l.length
      }


      // Back cursor so we never underline whitespaces
      while(l(end - 1).isWhitespace) {
        end -= 1
      }

      val pre = l.substring(0, start)
      val highlighted = l.substring(start, end)
      val post = l.substring(end, l.length)
      sb ++= pre ++ Console.UNDERLINED ++ errorStyle ++ highlighted ++ Console.RESET ++ post ++ "\n"
    } else {
      sb ++= "<line unavailable in source file>"
    }
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

  private def word: Parser[String] = """[a-zA-Z\d_]+""".r ^^ {
    _.toString
  }

  private def typeList: Parser[String] = ((Seperator ~ (word | template)) +) ^^ {
    case list => list.map(_._2).mkString(", ")
  }

  private def template: Parser[String] = StartEnd ~ word ~ typeList ~ StartEnd ^^ {
    case _ ~ word ~ args ~ _ => s"$word<$args>"
  }
}

