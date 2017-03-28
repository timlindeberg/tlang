package tlang.compiler.error

import tlang.utils.Colors.Color
import tlang.utils.Positioned

/**
  * Created by Tim Lindeberg on 1/14/2017.
  */
case class ErrorFormatter(error: ErrorMessage, formatting: Formatting, errorContextSize: Int) {

  import formatting._
  import formatting.colors._

  val NonColoredIndicationChar = "~"

  val ErrorColor: Color = {
    val color = error match {
      case _: Warning => Yellow
      case _          => Red
    }
    color + Bold
  }

  val pos  : Positioned         = error.pos
  val lines: IndexedSeq[String] = if (pos.hasSource) pos.source.text.lines.toIndexedSeq else IndexedSeq()

  def errorPrefix: String = {
    val pre = error match {
      case _: Warning => "Warning"
      case _: Error   => "Error"
      case _: Fatal   => "Fatal"
    }
    ErrorColor(pre + " " + error.code) + " "
  }

  def position: String = {
    val Style = Bold + NumColor
    Style(pos.line) + ":" + Style(pos.col)
  }

  def locationInFile: List[(String, String)] = {
    val ctxLines = contextLines
    val indent = getIndent(ctxLines)

    val lines = if (colors.isActive)
      ctxLines.map { case (lineNum, line) =>
        val marking = Marking(pos, Bold + Underline + ErrorColor, lineNum)
        (lineNum.toString, syntaxHighlighter(line, marking))
      }
    else
      ctxLines.flatMap { case (lineNum, line) =>
        indicatorLines(lineNum, line, indent)
      }

    lines.map { case (lineNum, line) => (NumColor(lineNum), if (line.isEmpty) "" else line.substring(indent)) }
  }

  def getIndent(ctxLines: List[(Int, String)]): Int = {
    val indents = ctxLines
      .filter { case (_, str) => str.exists(!_.isWhitespace) }
      .map { case (_, str) => str.indexWhere(!_.isWhitespace) }
    if (indents.isEmpty) 0 else indents.min
  }

  def contextLines: List[(Int, String)] = {
    val start = clamp(pos.line - errorContextSize, 1, lines.size)
    val end = clamp(pos.line + errorContextSize, 1, lines.size)
    (start to end)
      .map(i => (i, lines(i - 1)))
      .toList
  }

  def indicatorLines(lineNum: Int, line: String, indent: Int): List[(String, String)] = {
    val lines = List((lineNum.toString, line))
    if (lineNum != pos.line)
      return lines

    val start = pos.col - 1
    val end = if (pos.endLine == pos.line) pos.endCol - 1 else line.length
    val whitespaces = " " * (start - indent)
    val indicator = NonColoredIndicationChar * (end - start)
    lines :+ ("", whitespaces + indicator)
  }

  private def clamp(x: Int, min: Int, max: Int): Int = Math.min(Math.max(x, min), max)


}