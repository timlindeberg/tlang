package tcompiler
package utils

import java.io.File

import org.backuity.ansi.AnsiFormatter.FormattedHelper

import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.util.parsing.combinator.RegexParsers

class CompilationException(message: String) extends Exception(message)

class Reporter(quiet: Boolean = false) {

  var filesToLines = Map[File, IndexedSeq[String]]()
  var errors       = ArrayBuffer[String]()
  var warnings     = ArrayBuffer[String]()

  def warning(errorPrefix: String, errorCode: Int, msg: Any, pos: Positioned = NoPosition): Unit = {
    val warning = report(constructErrorPrefix(errorPrefix, 1, errorCode), msg, pos)
    warnings += warning
    if (!quiet)
      println(warning)
  }

  def error(errorPrefix: String, errorCode: Int, msg: String, pos: Positioned = NoPosition): Unit =
    errors += report(constructErrorPrefix(errorPrefix, 2, errorCode), msg, pos)


  def fatal(errorPrefix: String, errorCode: Int, msg: String, pos: Positioned = NoPosition): Nothing = {
    val error = report(constructErrorPrefix(errorPrefix, 3, errorCode), msg, pos)
    throw new CompilationException(error)
  }

  def clear() = {
    errors.clear()
    warnings.clear()
  }
  def hasErrors = errors.nonEmpty

  def terminateIfErrors() =
    if (hasErrors)
      throw new CompilationException(errors.mkString("\n\n"))


  private def constructErrorPrefix(errorPrefix: String, errorType: Int, errorCode: Int) = {
    val code = leftPad(errorCode)

    errorType match {
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


  private def report(prefix: String, msg: Any, pos: Positioned) = {
    errMessage(prefix, msg, pos)
  }


  private def errMessage(prefix: String, msg: Any, pos: Positioned): String = {
    val msgStr = handleQuoteLiterals(msg.toString) //replaceTemplateNames(msg.toString)
    var s = ""
    if (pos.hasPosition)
      s += filePrefix(pos)

    s += s" $prefix: $msgStr"

    if (pos.hasPosition)
      s += locationIndicator(pos)

    s
  }

  private def handleQuoteLiterals(msg: String) = {
    val citationColor = Console.MAGENTA
    val msgFormat = Console.RESET + Console.BOLD

    val re = """'(.+?)'""".r

    val s = re.replaceAllIn(msg, m => {
      val name = TemplateNameParser.parseTemplateName(m.group(1))
      "\'" + citationColor + name + msgFormat + "\'"
    })
    msgFormat + s + Console.RESET
  }


  private def filePrefix(pos: Positioned) = ansi"%bold{[}${pos.position}%bold{]}"

  private def locationIndicator(pos: Positioned) = {
    val lines = getLines(pos.file)
    var s = "\n"
    if (pos.line - 1 < lines.size) {
      val line = lines(pos.line - 1)
      val firstNonWhiteSpace = line.indexWhere(c => !c.isWhitespace)

      s += line.substring(firstNonWhiteSpace) + "\n"
      s += " " * (pos.col - 1 - firstNonWhiteSpace) + "^" + "\n"
    } else {
      s += "<line unavailable in source file>"
    }
    s
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
    if(first == -1)
      return s
    
    val last = s.lastIndexOf('-')
    if(last == -1)
      return s
    
    val preFix = s.substring(0, first)
    val middle = s.substring(first, last + 1)
    val postFix = s.substring(last + 1, s.length)

    parseAll(template, middle) match {
      case Success(res, _) => preFix + res + postFix
      case Failure(_, _)   => s
    }
  }

  // Grammar

  private def word: Parser[String] = """[a-zA-Z\d_]+""".r ^^ {_.toString }

  private def typeList: Parser[String] = ((Seperator ~ ( word | template )) +) ^^ {
    case list => list.map(_._2).mkString(", ")
  }

  private def template: Parser[String] = StartEndSign ~ word ~ typeList ~ StartEndSign ^^ {
    case _ ~ w ~ args ~ _ => s"$w<$args>"
  }
}

