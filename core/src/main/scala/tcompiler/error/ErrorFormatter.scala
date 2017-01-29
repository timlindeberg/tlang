package tcompiler.error

import java.io.File
import java.util.regex.Matcher

import tcompiler.Main
import tcompiler.utils.Extensions._
import tcompiler.utils.{Colorizer, Helpers}

import scala.collection.mutable
import scala.io.Source


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

/**
  * Created by Tim Lindeberg on 1/14/2017.
  */
case class ErrorFormatter(
  error: Error,
  colorizer: Colorizer,
  errorContextSize: Int) {

  import ErrorFormatter._
  import colorizer._

  private val QuoteColor   = Magenta
  private val MessageStyle = Bold
  private val WarningColor = Yellow + Bold
  private val ErrorColor   = Red + Bold
  private val FatalColor   = Red + Bold
  private val ErrorStyle   = {
    val color = error.errorLevel match {
      case ErrorLevel.Warning => WarningColor
      case ErrorLevel.Error   => ErrorColor
      case ErrorLevel.Fatal   => FatalColor
    }
    Underline + color
  }

  private val QuoteRegex        = """'(.+?)'""".r
  private val pos               = error.pos
  private val lines             = if (pos.hasFile) getLines(pos.file.get) else Nil
  private val syntaxHighlighter = new SyntaxHighlighter(colorizer)
  private val boxFormatting     = LightBoxFormatting
  private val wordWrapper       = new AnsiWordWrapper(colorizer)

  def format(): String = {
    import boxFormatting._
    val sb = new StringBuilder

    sb ++= ┌ + ─ * (LineWidth - 2) + ┐ + "\n"

    val validPosition = pos.hasFile && (1 to lines.size contains pos.line)

    if (validPosition)
      sb ++= filePrefix

    sb ++= formattedMessage


    if (validPosition)
      sb ++= locationInFile

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
      val fileName = pos.file.get.getName.replaceAll(Main.FileEnding, "")
      position = position.replaceAll("(\\d)", s"$Style$$1$Reset")
      position = position.replaceAll(fileName, s"$Style$fileName$Reset")
    }
    wordWrapper(position, LineWidth - 4).map(makeLine(_)).mkString
  }

  private def formattedMessage: String = {
    val msgFormat = Reset + MessageStyle

    val s = QuoteRegex.replaceAllIn(error.msg.toString, m => {
      var name = m.group(1)
      name = error.names.getOrElse(name, name)
      name = TemplateNameParser.parseTemplateName(name)
      Matcher.quoteReplacement("\'" + Reset + QuoteColor + name + msgFormat + "\'") // escape dollar signs etc.
    })
    val res = errorPrefix + msgFormat + s + Reset
    val wrapped = wordWrapper(res, LineWidth - 4)
    wrapped.map(makeLine(_)).mkString
  }

  private def makeLine(s: String, width: Int = LineWidth - 4): String = {
    import boxFormatting._

    val whitespaces = " " * (width - s.charCount)
    │ + " " + s + whitespaces + " " + │ + "\n"
  }

  private def locationInFile: String = {
    import boxFormatting._

    val ctxLines = contextLines
    val digits = ctxLines.map { case (i, _) => numDigits(i) }.max

    val sb = new StringBuilder
    sb ++= seperator(├, ┬, ┤, digits)

    val indents = ctxLines
      .filter { case (_, str) => str.exists(!_.isWhitespace) }
      .map { case (_, str) => str.indexWhere(!_.isWhitespace) }
    val indent = if (indents.isEmpty) 0 else indents.min

    ctxLines.foreach { case (i, line) =>
      val str = if (useColor) colorLine(i, line) else noColorLine(i, line, indent, digits)
      val s = if (str.isEmpty) "" else str.substring(indent)

      var first = true
      val len = LineWidth - digits - 7
      wordWrapper(s, len).foreach { l =>
        val lineNum = if (first) i else -1
        first = false
        sb ++= lineNumPrefix(lineNum, digits) + makeLine(l, len)
      }
    }

    sb ++= seperator(└, ┴, ┘, digits)
    sb.toString()
  }

  private def contextLines = {
    val start = Helpers.clamp(pos.line - errorContextSize, 1, lines.size)
    val end = Helpers.clamp(pos.line + errorContextSize, 1, lines.size)
    (start to end)
      .map(i => (i, lines(i - 1)))
      .toList
  }

  private def colorLine(lineNum: Int, line: String): String = {
    if (lineNum < pos.line || lineNum > pos.endLine)
      return syntaxHighlighter(line)

    val firstWhiteSpace = line.indexWhere(!_.isWhitespace)
    val start = if (lineNum == pos.line) pos.col - 1 else firstWhiteSpace
    val end = if (lineNum == pos.endLine) pos.endCol - 1 else line.length
    val pre = syntaxHighlighter(line.substring(0, start))
    val highlighted = line.substring(start, end)
    val post = syntaxHighlighter(line.substring(end, line.length))

    val whitespace = " " * firstWhiteSpace
    whitespace + pre + ErrorStyle + highlighted + Reset + post
  }

  private def noColorLine(lineNum: Int, line: String, indent: Int, digits: Int): String = {
    if (lineNum != pos.line)
      return line

    val start = pos.col - 1
    val end = if (pos.endLine == pos.line) pos.endCol - 1 else line.length
    val whitespaces = " " * digits + "| " + " " * (start - indent)
    val indicator = NonColoredIndicationChar * (end - start)
    line + "\n" + whitespaces + indicator
  }

  private def lineNumPrefix(lineNumber: Int, digits: Int) = {
    import boxFormatting._
    val digitsInLineNum = if (lineNumber == -1) 0 else numDigits(lineNumber)
    val whiteSpaces = " " * (digits - digitsInLineNum)
    val sb = new StringBuilder
    sb ++= │ + " "
    if (lineNumber != -1)
      sb ++= NumColor + lineNumber.toString + Reset
    sb ++= whiteSpaces + " "
    sb.toString()
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

  private def seperator(left: String, bridge: String, right: String, digits: Int) = {
    import boxFormatting._

    val rest = ─ * (LineWidth - digits - 5)
    val overNumbers = ─ * (digits + 2)
    left + overNumbers + bridge + rest + right + "\n"
  }

}