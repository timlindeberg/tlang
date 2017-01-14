package tcompiler.error

import java.io.File
import java.util.regex.Matcher

import tcompiler.Main
import tcompiler.utils.Colored

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

  override def NumColor: String = Blue
  def QuoteColor: String = Magenta
  def MessageStyle: String = Bold
  def WarningColor: String = Yellow
  def ErrorColor: String = Red + Bold
  def FatalColor: String = Red


  private val sb                        = new StringBuilder()
  private val pos                       = error.pos
  private val errorLetters              = error.letters
  private val lines: IndexedSeq[String] = getLines(pos.file)

  def format(): String = {
    val prefix = errorPrefix
    val msgStr = formatMessage


    val validPosition = pos.hasPosition && (1 to lines.size contains pos.line)

    if (validPosition)
      sb ++= filePrefix + "\n"

    sb ++= s"$prefix: $msgStr\n"


    if (validPosition) {
      sb ++= CodeSeperator
      sb ++= locationInFile
      sb ++= CodeSeperator
    }

    sb.toString()
  }

  private def errorPrefix: String = {
    val code = leftPad(error.code)

    error.errorLevel match {
      case ErrorLevel.Warning => s"${WarningColor}Warning$Reset ($WarningColor${errorLetters}1$code$Reset)"
      case ErrorLevel.Error   => s"${ErrorColor}Error$Reset ($ErrorColor${errorLetters}2$code$Reset)"
      case ErrorLevel.Fatal   => s"${FatalColor}Fatal$Reset ($FatalColor${errorLetters}3$code$Reset)"
    }
  }

  private def leftPad(num: Int): String = num match {
    case x if x >= 0 && x < 10     => "00" + x
    case x if x >= 10 && x < 100   => "0" + x
    case x if x >= 100 && x < 1000 => "" + x
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

  private val QuoteRegex = """'(.+?)'""".r
  private def formatMessage: String = {
    val msgFormat = Reset + MessageStyle


    val s = QuoteRegex.replaceAllIn(error.msg.toString, m => {
      var name = m.group(1)
      name = error.names.getOrElse(name, name)
      name = TemplateNameParser.parseTemplateName(name)
      Matcher.quoteReplacement("\'" + QuoteColor + name + msgFormat + "\'") // escape dollar signs etc.
    })
    msgFormat + s + Reset
  }

  private def locationInFile: String = {
    takeLines(errorContext, before = true)
    sb ++= lineNumPrefix(pos.line)

    val line = lines(pos.line - 1)
    val start = pos.col - 1
    val end = getEndCol(line)

    if (useColor)
      addColoredLine(line, start, end)
    else
      addNonColoredLine(line, start, end)

    sb ++= "\n"
    takeLines(errorContext, before = false)

    sb.toString
  }

  private def addNonColoredLine(line: String, start: Int, end: Int) = {
    val prefixLength = MaxLineNumberDigits + 1
    sb ++= line
    sb ++= "\n"
    sb ++= " " * (prefixLength + start)
    sb ++= "~" * (end - start)
  }

  private def addColoredLine(line: String, start: Int, end: Int) = {
    val pre = line.substring(0, start)
    val highlighted = line.substring(start, end)
    val post = line.substring(end, line.length)
    sb ++= pre
    sb ++= errorStyle
    sb ++= highlighted
    sb ++= Reset
    sb ++= post
  }

  private def lineNumPrefix(line: Int) =
    s"$Bold$NumColor$line$Reset:" + " " * (MaxLineNumberDigits - (line / 10))

  private def takeLines(numLines: Int, before: Boolean) = {
    val start = if (before) pos.line - numLines else pos.line + 1
    val end = start + numLines
    for (i <- start until end if i >= 1 && i <= lines.size) {
      sb ++= lineNumPrefix(i)
      sb ++= lines(i - 1)
      sb ++= "\n"
    }
  }

  private def errorStyle = {
    val color = error.errorLevel match {
      case ErrorLevel.Warning => WarningColor
      case ErrorLevel.Error   => ErrorColor
      case ErrorLevel.Fatal   => FatalColor
    }
    Underline + color
  }


  private def getEndCol(line: String): Int = {
    var end = if (pos.endLine == pos.line) {
      pos.endCol - 1
    } else {
      val firstSlash = line.indexOf("//")
      if (firstSlash != -1)
        firstSlash - 1
      else
        line.length
    }


    // Back cursor so we never underline whitespaces
    while (line(end - 1).isWhitespace)
      end -= 1
    end
  }


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

  val MaxLineNumberDigits                                  = 6
  val CodeSeperator: String                                = "-" * 80 + "\n"
  val LineCache    : mutable.Map[File, IndexedSeq[String]] = mutable.Map()

  def getLines(f: File): IndexedSeq[String] =
    LineCache.getOrElseUpdate(f, {
      val source = Source.fromFile(f).withPositioning(true)
      val lines = source.getLines().toIndexedSeq
      source.close()
      lines
    })

}