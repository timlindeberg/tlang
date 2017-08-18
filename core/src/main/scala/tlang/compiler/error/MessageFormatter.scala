package tlang.compiler.error

import java.io.File

import tlang.compiler.options.Flags.MessageContext
import tlang.utils.formatting.Colors.Color
import tlang.utils.formatting.{Formatter, Marking}
import tlang.utils.{FileSource, Positioned}

case class MessageFormatter(
  formatter: Formatter,
  messageContextSize: Int = MessageContext.defaultValue,
  tabWidth: Int = 2) {

  import formatter.formatting._

  val TabSpaces              = " " * tabWidth
  var error: CompilerMessage = _

  def setError(error: CompilerMessage): Unit = this.error = error

  def color: Color = error.messageType.color(formatter.formatting) + Bold

  def pos: Positioned = error.pos
  def lines: IndexedSeq[String] = if (pos.hasSource) pos.source.text.lines.toIndexedSeq else IndexedSeq()

  def prefix: String = color(error.messageType.name + " " + error.code) + " "

  def position: String = {
    val Style = Bold + NumColor
    Style(pos.line) + ":" + Style(pos.col)
  }

  def sourceDescription: String = {
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
      if (formatter.formatting.useColor)
        ctxLines.map { case (lineNum, line) =>
          val marking = Marking(pos, Bold + Underline + color, lineNum)
          val coloredLine = formatter.syntaxHighlight(line, marking)
          (lineNum.toString, replaceTabs(coloredLine))
        }
      else
        ctxLines.flatMap { case (lineNum, line) =>
          indicatorLines(lineNum, line)

        }

    lines.map { case (lineNum, line) =>
      val trimmedLine = if (line.isEmpty) line else line.substring(indent)
      (NumColor(lineNum), trimmedLine)
    }
  }

  def replaceTabs(s: String): String = s.replaceAll("\t", TabSpaces)

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
    val start = clamp(pos.line - messageContextSize, 1, lines.size)
    val end = clamp(pos.line + messageContextSize, 1, lines.size)
    (start to end)
      .map(i => (i, lines(i - 1)))
      .toList
  }

  def indicatorLines(lineNum: Int, line: String): List[(String, String)] = {
    val lines = List((lineNum.toString, replaceTabs(line)))
    if (lineNum != pos.line)
      return lines

    val start = pos.col - 1
    val end = if (pos.endLine == pos.line) pos.endCol - 1 else line.length


    val indicator = UnderlineCharacter * (end - start)
    lines :+ ("", indentationOfIndicator(line, start) + indicator)
  }

  private def indentationOfIndicator(line: String, start: Int): String = {
    val whitespaces = new StringBuilder
    var i = 0
    while (i < start) {
      whitespaces ++= (if (line(i) == '\t') TabSpaces else " ")
      i += 1
    }
    whitespaces.toString
  }

  private def clamp(x: Int, min: Int, max: Int): Int = Math.min(Math.max(x, min), max)


}
