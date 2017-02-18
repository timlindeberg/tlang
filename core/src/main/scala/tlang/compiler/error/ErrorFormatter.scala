package tlang.compiler.error

import tlang.utils.Colors.Color


/**
  * Created by Tim Lindeberg on 1/14/2017.
  */
case class ErrorFormatter(error: Error, formatting: Formatting, errorContextSize: Int) {

  import formatting._
  import formatting.colors._

  private val NonColoredIndicationChar = "~"

  private val ErrorColor: Color =
    error.errorLevel match {
      case ErrorLevel.Warning => Yellow + Bold
      case ErrorLevel.Error   => Red + Bold
      case ErrorLevel.Fatal   => Red + Bold
    }

  private val pos               = error.pos
  private val lines             = pos.source.text.lines.toIndexedSeq
  private val syntaxHighlighter = SyntaxHighlighter(formatting.colors)

  def format(): String = {
    val sb = new StringBuilder

    sb ++= top

    val validPosition = 1 to lines.size contains pos.line

    if (validPosition)
      sb ++= sourceDescription

    sb ++= makeLines(errorPrefix + error.msg)


    if (validPosition)
      sb ++= makeBlockWithColumn(locationInFile)
    else
      sb ++= bottom

    sb.toString()
  }

  private def errorPrefix: String = {
    val pre = error.errorLevel match {
      case ErrorLevel.Warning => "Warning"
      case ErrorLevel.Error   => "Error"
      case ErrorLevel.Fatal   => "Fatal"
    }
    ErrorColor(pre + " " + error.code) + ": "
  }

  private def sourceDescription: String = {
    val Style = Bold + NumColor
    val description = pos.source.description(formatting) + Style(pos.line) + ":" + Style(pos.col)
    makeLines(description)
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
    val start = clamp(pos.line - errorContextSize, 1, lines.size)
    val end = clamp(pos.line + errorContextSize, 1, lines.size)
    (start to end)
      .map(i => (i, lines(i - 1)))
      .toList
  }

  private def clamp(x: Int, min: Int, max: Int): Int = Math.min(Math.max(x, min), max)


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