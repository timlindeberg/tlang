package tlang
package compiler
package messages

import tlang.formatting.Colors.Color
import tlang.formatting.Formatter
import tlang.formatting.textformatters.SyntaxHighlighter
import tlang.options.argument.MessageContextFlag

import tlang.utils.{Position, Positioned}

case class MessageInfo(
  message: CompilerMessage,
  syntaxHighlighter: SyntaxHighlighter,
  messageContextSize: Int = MessageContextFlag.defaultValue
)(
  implicit formatter: Formatter
) {

  private val lines: IndexedSeq[String] = message.pos.source.map(_.lines).getOrElse(IndexedSeq())

  import formatter._


  def color: Color = message.messageType.color + Bold
  def position: Positioned = message.pos

  def hasValidPosition: Boolean = position.source.nonEmpty && (position.line in (1 to lines.size + 1))

  def prefix: String = color(message.messageType.name + " " + message.code)

  def positionDescription: String = {
    val Style = Bold + NumColor
    Style(position.line) + ":" + Style(position.col)
  }

  def sourceDescription: String = {
    val sourceDescription = position.source match {
      case Some(source) => source.description
      case _            => ""
    }
    s"$sourceDescription:$positionDescription"
  }

  def locationInSource: Seq[(String, String)] = {

    val (trimmedLines, trimmedPos) = trimIndent(contextLines)
    val (lines, adjustedPos) = formatter.replaceTabs(trimmedLines, trimmedPos)

    if (formatter.useColor)
      lines.map { case (line, lineNum) => coloredIndicatorLine(lineNum, line, adjustedPos) }
    else
      lines.flatMap { case (line, lineNum) => indicatorLines(lineNum, line, adjustedPos) }
  }

  private def contextLines: Seq[(String, Int)] = {
    val start = (position.line - messageContextSize).clamp(1, lines.size)
    val end = (position.line + messageContextSize).clamp(1, lines.size)
    (start to end)
      .map(i => (lines(i - 1), i))
      .toList
  }

  private def trimIndent(lines: Seq[(String, Int)]): (Seq[(String, Int)], Position) = {
    val indent = getMinimumIndent(lines)

    val trimmedLines = lines.map { case (line, lineNum) =>
      val trimmedLine = if (line.nonEmpty) line.substring(indent) else ""
      (trimmedLine, lineNum)
    }
    val start = math.max(1, position.col - indent)
    val end = math.max(1, position.colEnd - indent)
    val adjustedPos = Position(position.line, start, position.lineEnd, end)
    (trimmedLines, adjustedPos)
  }

  private def getMinimumIndent(lines: Seq[(String, Int)]): Int = {
    val nonEmptyLines = lines
      .filter { case (line, _) => line.nonEmpty }
      .map { case (line, _) => indent(line) }

    if (nonEmptyLines.isEmpty) 0 else nonEmptyLines.min
  }

  private def indent(line: String): Int = {
    var i = 0
    var indent = 0
    while (i < line.length && line(i).isWhitespace) {
      indent += 1
      i += 1
    }
    indent
  }

  private def coloredIndicatorLine(lineNum: Int, line: String, pos: Positioned): (String, String) = {
    val highlightedLine = if (lineNum in (pos.line to pos.lineEnd)) {
      val MarkColor = Underline + color
      val (start, end) = startAndEnd(line, lineNum, pos)
      line.substring(0, start) + MarkColor(line.substring(start, end)) + line.substring(end, line.length)
    } else {
      line
    }

    (NumColor(lineNum), syntaxHighlighter(highlightedLine))
  }

  private def indicatorLines(lineNum: Int, line: String, pos: Positioned): List[(String, String)] = {
    val firstLine = (lineNum.toString, line)
    if (lineNum notIn (pos.line to pos.lineEnd))
      return firstLine :: Nil

    val (start, end) = startAndEnd(line, lineNum, pos)
    val indicator = UnderlineCharacter * (end - start)
    val indentation = " " * start
    firstLine :: ("", indentation + indicator) :: Nil
  }

  private def startAndEnd(line: String, lineNum: Int, pos: Positioned): (Int, Int) = {
    val startOfText = line.indexWhere(!_.isWhitespace)
    val start = if (lineNum == pos.line) pos.col - 1 else 0
    val end = if (lineNum == pos.lineEnd) pos.colEnd - 1 else line.length
    (Math.max(startOfText, start), Math.min(end, line.length))
  }
}
