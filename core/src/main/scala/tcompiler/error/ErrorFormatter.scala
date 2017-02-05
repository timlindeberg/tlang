package tcompiler.error

import java.io.File
import java.util.regex.Matcher

import tcompiler.Main
import tcompiler.utils.Helpers

import scala.io.Source


/**
  * Created by Tim Lindeberg on 1/14/2017.
  */
case class ErrorFormatter(
  error: Error,
  formatting: Formatting,
  errorContextSize: Int) {

  import formatting._
  import formatting.colors._

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

  private val NonColoredIndicationChar = "~"
  private val QuoteRegex               = """'(.+?)'""".r
  private val pos                      = error.pos
  private val lines                    = if (pos.hasFile) getLines(pos.file.get) else Nil
  private val syntaxHighlighter        = SyntaxHighlighter(formatting.colors)

  def format(): String = {
    val sb = new StringBuilder

    sb ++= top

    val validPosition = pos.hasFile && (1 to lines.size contains pos.line)

    if (validPosition)
      sb ++= filePrefix

    sb ++= formattedMessage


    if (validPosition)
      sb ++= makeBlockWithColumn(locationInFile)
    else
      sb ++= bottom

    sb.toString()
  }

  private def getLines(f: File): IndexedSeq[String] = {
    val source = Source.fromFile(f)
    val lines = source.getLines().toIndexedSeq
    source.close()
    lines
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
    if (active) {
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

  private def locationInFile: List[(String, String)] = {
    val ctxLines = contextLines
    val indent = getIndent(ctxLines)

    val lines = if (colors.active)
      ctxLines.map { case (lineNum, line) =>
        (lineNum.toString, syntaxHighlighter(line, Marking(lineNum, pos, ErrorStyle)))
      }
    else
      ctxLines.flatMap { case (lineNum, line) =>
        indicatorLines(lineNum, line, indent)
      }

    lines.map { case (lineNum, line) => (NumColor(lineNum), if (line.isEmpty) "" else line.substring(indent)) }
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

  private def indicatorLines(lineNum: Int, line: String, indent: Int): List[(String, String)] = {
    val lines = List((lineNum.toString, line))
    if (lineNum != pos.line)
      return lines

    val start = pos.col - 1
    val end = if (pos.endLine == pos.line) pos.endCol - 1 else line.length
    val whitespaces = " " * (start - indent)
    val indicator = NonColoredIndicationChar * (end - start)
    lines :+ ("", whitespaces + indicator)
  }

}