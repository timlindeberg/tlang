package tcompiler.error

import java.io.File
import java.util.regex.Matcher

import org.apache.commons.lang3.text.WordUtils
import tcompiler.Main
import tcompiler.utils.Colored
import tcompiler.utils.Extensions._

import scala.collection.mutable
import scala.io.Source
import scala.util.parsing.combinator.RegexParsers

/**
  * Created by Tim Lindeberg on 1/14/2017.
  */
case class ErrorFormatter(
  error: Error,
  override val useColor: Boolean,
  errorContext: Int) extends Colored {

  import ErrorFormatter._


  override val NumColor     = Blue
  private  val QuoteColor   = Magenta
  private  val MessageStyle = Bold
  private  val WarningColor = Yellow
  private  val ErrorColor   = Red + Bold
  private  val FatalColor   = Red
  private  val ErrorStyle   = {
    val color = error.errorLevel match {
      case ErrorLevel.Warning => WarningColor
      case ErrorLevel.Error   => ErrorColor
      case ErrorLevel.Fatal   => FatalColor
    }
    Underline + color
  }

  private val QuoteRegex       = """'(.+?)'""".r
  private val sb               = new StringBuilder()
  private val pos              = error.pos
  private val lines            = getLines(pos.file)
  private val maxLineNumDigits = numDigits(clamp(pos.line + errorContext, 1, lines.size)) + 1

  def format(): String = {
    val prefix = errorPrefix
    val msgStr = formatMessage(prefix.charCount)


    val validPosition = pos.hasPosition && (1 to lines.size contains pos.line)

    if (validPosition)
      sb ++= filePrefix + "\n"

    sb ++= s"$prefix$msgStr\n"


    if (validPosition)
      addLocationInFile()

    sb.toString()
  }

  private def errorPrefix: String = {
    val pre = error.errorLevel match {
      case ErrorLevel.Warning => s"${WarningColor}Warning "
      case ErrorLevel.Error   => s"${ErrorColor}Error "
      case ErrorLevel.Fatal   => s"${FatalColor}Fatal "
    }
    pre + error.code + Reset + ": "
  }

  private def filePrefix: String = {
    val Style = Bold + NumColor
    var position = pos.position
    if (useColor) {
      val fileName = pos.file.getName.replaceAll(Main.FileEnding, "")
      position = position.replaceAll("(\\d)", s"$Style$$1$Reset")
      position = position.replaceAll(fileName, s"$Style$fileName$Reset")
    }

    s"$Bold[$Reset$position$Bold]$Reset"
  }

  private def formatMessage(prefixLength: Int): String = {
    val msgFormat = Reset + MessageStyle

    val newLine = "\n" + " " * prefixLength
    val wrapped = WordUtils.wrap(error.msg.toString, LineWidth - prefixLength, newLine, true)

    val s = QuoteRegex.replaceAllIn(wrapped, m => {
      var name = m.group(1)
      name = error.names.getOrElse(name, name)
      name = TemplateNameParser.parseTemplateName(name)
      Matcher.quoteReplacement("\'" + QuoteColor + name + msgFormat + "\'") // escape dollar signs etc.
    })
    msgFormat + s + Reset
  }

  private def addLocationInFile(): Unit = {
    sb ++= CodeSeperator

    val ctxLines = contextLines

    val indent = ctxLines.map { case (_, str) =>
      val i = str.indexWhere(!_.isWhitespace)
      if (i == -1) 0 else i
    }.min

    ctxLines.foreach { case (i, line) =>
      val str = if (i == pos.line) indicator(line, indent) else line

      sb ++= lineNumPrefix(i)
      sb ++= str.substring(indent) + "\n"
    }

    sb ++= CodeSeperator
  }

  private def contextLines = {
    val start = clamp(pos.line - errorContext, 1, lines.size)
    val end = clamp(pos.line + errorContext, 1, lines.size)
    (start to end)
      .map(i => (i, lines(i - 1)))
      .toList
  }

  private def indicator(line: String, indent: Int) = {
    val start = pos.col - 1
    val end = getEndPos(line, start, indent)
    if (useColor) {
      val pre = line.substring(0, start)
      val highlighted = line.substring(start, end)
      val post = line.substring(end, line.length)
      s"$pre$ErrorStyle$highlighted$Reset$post"
    } else {
      val prefixLength = maxLineNumDigits + 1 - indent
      val whitespace = " " * (prefixLength + start)
      val indicator = NonColoredIndicationChar * (end - start)
      s"$line\n$whitespace$indicator"
    }
  }

  private def lineNumPrefix(line: Int) =
    s"$Bold$NumColor$line$Reset|" + " " * (maxLineNumDigits - numDigits(line))

  private def getEndPos(line: String, start: Int, indent: Int): Int = {
    var end = if (pos.endLine == pos.line) {
      pos.endCol - 1
    } else {
      val commentIndexes = line.allIndexesOf("//") ::: line.allIndexesOf("/*")
      commentIndexes.filter(_ > start).sorted.headOption.getOrElse(pos.endCol)
    }

    // Back cursor so we never underline whitespaces
    while (end > 1 && line(end - 1).isWhitespace)
      end -= 1
    end
  }

  private def numDigits(num: Int): Int = {
    var n = 1
    var i = num
    if (i >= 100000000) {n += 8; i /= 100000000}
    if (i >= 10000) {n += 4; i /= 10000}
    if (i >= 100) {n += 2; i /= 100}
    if (i >= 10) {n += 1;}
    n
  }

  private def clamp(x: Int, min: Int, max: Int) = Math.min(Math.max(x, min), max)

  private object TemplateNameParser extends RegexParsers {

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
        case NoSuccess(_, _) => s
        case Failure(_, _)   => s
      }
    }

    // Grammar

    private def word: Parser[String] =
      """[a-zA-Z\d_]+""".r ^^ {
        _.toString
      }

    private def typeList: Parser[String] = ((Seperator ~ (word | template)) +) ^^ (list => list.map(_._2).mkString(", "))

    private def template: Parser[String] = StartEnd ~ word ~ typeList ~ StartEnd ^^ {
      case _ ~ word ~ args ~ _ => s"$word<$args>"
    }
  }


}

object ErrorFormatter {

  val LineWidth                                            = 80
  val CodeSeperator: String                                = "-" * LineWidth + "\n"
  val NonColoredIndicationChar                             = "~"
  val LineCache    : mutable.Map[File, IndexedSeq[String]] = mutable.Map()

  def getLines(f: File): IndexedSeq[String] =
    LineCache.getOrElseUpdate(f, {
      val source = Source.fromFile(f).withPositioning(true)
      val lines = source.getLines().toIndexedSeq
      source.close()
      lines
    })

}