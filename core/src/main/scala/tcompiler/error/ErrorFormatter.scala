package tcompiler.error

import java.io.File
import java.util.regex.Matcher

import tcompiler.Main
import tcompiler.utils.Helpers

import scala.collection.mutable
import scala.io.Source

object ErrorFormatter {

  val NonColoredIndicationChar                         = "~"
  val LineCache: mutable.Map[File, IndexedSeq[String]] = mutable.Map()

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
  formatting: Formatting,
  errorContextSize: Int) {

  import ErrorFormatter._
  import formatting._
  import formatting.box._
  import formatting.colorizer._

  private val QuoteColor   = Magenta
  private val MessageStyle = Bold
  private val WarningColor = Yellow + Bold
  private val ErrorColor   = Red + Bold
  private val FatalColor   = Red + Bold

  private val ErrorStyle: String = {
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
  private val syntaxHighlighter = new SyntaxHighlighter(formatting.colorizer)
  private val wordWrapper       = new AnsiWordWrapper

  def format(): String = {
    val sb = new StringBuilder

    sb ++= top

    val validPosition = pos.hasFile && (1 to lines.size contains pos.line)

    if (validPosition)
      sb ++= filePrefix

    sb ++= formattedMessage


    if (validPosition)
      sb ++= locationInFile
    else
      sb ++= bottom

    sb.toString()
  }

  private def errorPrefix: String = {
    val pre = error.errorLevel match {
      case ErrorLevel.Warning => WarningColor + "Warning"
      case ErrorLevel.Error   => ErrorColor + "Error"
      case ErrorLevel.Fatal   => FatalColor + " Fatal"
    }
    pre + " " + error.code + Reset + ": "
  }

  private def filePrefix: String = {
    val Style = Bold + NumColor
    var position = pos.position
    if (useColor) {
      val fileName = pos.file.get.getName.replaceAll(Main.FileEnding, "")
      position = position.replaceAll("(\\d)", s"$Style$$1$Reset")
      position = position.replaceAll(fileName, s"$Style$fileName$Reset")
    }
    makeLines(position)
  }

  private def formattedMessage: String = {
    val msgFormat = Reset + MessageStyle

    val s = QuoteRegex.replaceAllIn(error.msg.toString, m => {
      var name = m.group(1)
      name = error.names.getOrElse(name, name)
      name = TemplateNameParser.parseTemplateName(name)
      Matcher.quoteReplacement("\'" + Reset + QuoteColor + name + msgFormat + "\'") // escape dollar signs etc.
    })
    makeLines(errorPrefix + msgFormat + s + Reset)
  }

  private def locationInFile: String = {
    val ctxLines = contextLines
    val digits = ctxLines.map { case (i, _) => numDigits(i) }.max

    val sb = new StringBuilder
    sb ++= seperator(├, ┬, ┤, digits)

    val indent = getIndent(ctxLines)
    val lineLength = formatting.width - digits - 3

    val trimmed = ctxLines.map { case (num, line) =>
      val t = if (line.isEmpty) "" else line.substring(indent)
      (num, t)
    }

    val lines = if (useColor)
      trimmed.map { case (num, line) => (num, syntaxHighlighter(line, Marking(num, pos, ErrorStyle))) }
    else
      trimmed.flatMap { case (num, line) => indicatorLines(num, line, indent) }

    sb ++= lines
      .flatMap { case (lineNum, line) =>
        val lines = wordWrapper(line, lineLength)
        val lineNumbers = lineNum :: List.fill(lines.size - 1)(-1)
        lineNumbers.zip(lines)
      }
      .map { case (i, line) => lineNumPrefix(i, digits) + makeLine(line, lineLength) }
      .mkString


    sb ++= seperator(└, ┴, ┘, digits)
    sb.toString()
  }

  private def getIndent(ctxLines: List[(Int, String)]): Int = {
    val indents = ctxLines
      .filter { case (_, str) => str.exists(!_.isWhitespace) }
      .map { case (_, str) => str.indexWhere(!_.isWhitespace) }
    if (indents.isEmpty) 0 else indents.min
  }

  private def contextLines: List[(Int, String)] = {
    val start = Helpers.clamp(pos.line - errorContextSize, 1, lines.size)
    val end = Helpers.clamp(pos.line + errorContextSize, 1, lines.size)
    (start to end)
      .map(i => (i, lines(i - 1)))
      .toList
  }

  private def indicatorLines(lineNum: Int, line: String, indent: Int): List[(Int, String)] = {
    val lines = List((lineNum, line))
    if (lineNum != pos.line)
      return lines

    val start = pos.col - 1
    val end = if (pos.endLine == pos.line) pos.endCol - 1 else line.length
    val whitespaces = " " * (start - indent)
    val indicator = NonColoredIndicationChar * (end - start)
    lines :+ (-1, whitespaces + indicator)
  }

  private def lineNumPrefix(lineNumber: Int, digits: Int) = {
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

}