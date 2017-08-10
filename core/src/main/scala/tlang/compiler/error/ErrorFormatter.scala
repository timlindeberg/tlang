package tlang.compiler.error

import java.io.File

import tlang.utils.formatting.Colors.Color
import tlang.utils.formatting.{Formatting, Marking}
import tlang.utils.{FileSource, Positioned}

case class ErrorFormatter(error: ErrorMessage, formatting: Formatting, errorContextSize: Int, tabWidth: Int = 2) {

  import formatting._

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

  def sourceDescription = {
    val file = error.pos.source.asInstanceOf[FileSource].file
    val fileNameStyle = Bold + NumColor
    val fileName = fileNameStyle(file.getName)
    val fileDescription = file.getParent + File.separator + fileName
    position + " " + fileDescription
  }

  def locationInFile: List[(String, String)] = {
    val ctxLines = contextLines
    val indent = getMinimumIndent(ctxLines)

    val lines =
      if (formatting.useColor)
        ctxLines.map { case (lineNum, line) =>
          val marking = Marking(pos, Bold + Underline + ErrorColor, lineNum)
          val coloredLine = syntaxHighlighter(line, marking)
          (lineNum.toString, replaceTabs(coloredLine))
        }
      else
        ctxLines.flatMap { case (lineNum, line) =>
          indicatorLines(lineNum, replaceTabs(line), indent)
        }

    lines.map { case (lineNum, line) =>
      val trimmedLine = if (line.isEmpty) line else line.substring(indent)
      (NumColor(lineNum), trimmedLine)
    }
  }

  def replaceTabs(s: String): String = s.replaceAll("\t", " " * tabWidth)

  def getMinimumIndent(ctxLines: List[(Int, String)]): Int =
    ctxLines
      .filter { case (_, line) => line.nonEmpty }
      .map { case (_, line) => indent(line) }.min

  def indent(line: String): Int = {
    var i = 0
    var indent = 0
    while (i < line.length && line(i).isWhitespace) {
      if (line(i) == '\t')
        indent += tabWidth
      else
        indent += 1
      i += 1
    }
    indent
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
