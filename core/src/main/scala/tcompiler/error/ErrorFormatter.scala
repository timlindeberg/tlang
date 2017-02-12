package tcompiler.error

import java.io.File

import tcompiler.Main
import tcompiler.utils.Colors.Color
import tcompiler.utils.Extensions._
import tcompiler.utils.Helpers

import scala.collection.mutable
import scala.io.Source


object ErrorFormatter {

  val NonColoredIndicationChar = "~"

  private val LineCache: mutable.Map[File, IndexedSeq[String]] = mutable.Map()
  def getLines(file: File): IndexedSeq[String] =
    LineCache.getOrElseUpdate(file, using(Source.fromFile(file)) {_.getLines().toIndexedSeq})

}

/**
  * Created by Tim Lindeberg on 1/14/2017.
  */
case class ErrorFormatter(error: Error, formatting: Formatting, errorContextSize: Int) {

  import ErrorFormatter._
  import formatting._
  import formatting.colors._

  private val QuoteColor   = Magenta
  private val MessageStyle = Bold

  private val ErrorColor: Color =
    error.errorLevel match {
      case ErrorLevel.Warning => Yellow + Bold
      case ErrorLevel.Error   => Red + Bold
      case ErrorLevel.Fatal   => Red + Bold
    }

  private val QuoteRegex        = """'(.+?)'""".r
  private val pos               = error.pos
  private val lines             = if (pos.hasFile) getLines(pos.file.get) else Nil
  private val syntaxHighlighter = SyntaxHighlighter(formatting.colors)

  def format(): String = {
    val sb = new StringBuilder

    sb ++= top

    val validPosition = pos.hasFile && (1 to lines.size contains pos.line)

    if (validPosition)
      sb ++= filePrefix

    sb ++= makeLines(errorPrefix + error.msg)


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
      case ErrorLevel.Warning => "Warning"
      case ErrorLevel.Error   => "Error"
      case ErrorLevel.Fatal   => "Fatal"
    }
    ErrorColor(pre + " " + error.code) + ": "
  }

  private def filePrefix: String = {
    var position = pos.position
    if (colors.isActive) {
      val Style = Bold + NumColor
      val fileName = pos.file.get.getName.replaceAll(Main.FileEnding, "")
      position = position.replaceAll("(\\d)", s"$Style$$1$Reset")
      position = position.replaceAll(fileName, s"$Style$fileName$Reset")
    }
    makeLines(position)
  }

  private def locationInFile: List[(String, String)] = {
    val ctxLines = contextLines
    val indent = getIndent(ctxLines)

    val lines = if (colors.isActive)
      ctxLines.map { case (lineNum, line) =>
        (lineNum.toString, syntaxHighlighter(line, Marking(lineNum, pos, Underline + ErrorColor)))
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