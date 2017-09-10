package tlang.messages

import tlang.formatting.Colors.Color
import tlang.formatting.Formatter
import tlang.options.arguments.MessageContextFlag
import tlang.utils.Extensions._
import tlang.utils.{Position, Positioned}

case class MessageFormatter(
  formatter: Formatter,
  messageContextSize: Int = MessageContextFlag.defaultValue,
  tabWidth: Int = 2) {

  var lines: IndexedSeq[String] = IndexedSeq()

  // This is lazy so that the class can be mocked
  private lazy val formatting               = formatter.formatting
  private      val TabSpaces                = " " * tabWidth
  private      var message: CompilerMessage = _


  def setMessage(message: CompilerMessage): Unit = {
    this.message = message
    lines = message.pos.source match {
      case Some(source) => source.lines
      case None         => IndexedSeq()
    }
  }

  def hasValidPosition: Boolean = position.source.nonEmpty && (position.line in (1 to lines.size))

  def color: Color = message.messageType.color(formatting) + formatting.Bold

  def position: Positioned = message.pos

  def prefix: String = color(message.messageType.name + " " + message.code)

  def positionDescription: String = {
    import formatting._
    val Style = Bold + NumColor
    Style(position.line) + ":" + Style(position.col)
  }

  def sourceDescription: String = {
    val sourceDescription = position.source match {
      case Some(source) => source.description(formatting)
      case _            => ""
    }
    positionDescription + " " + sourceDescription
  }

  def locationInSource: List[(String, String)] = {

    val (trimmedLines, trimmedPos) = trimIndent(contextLines)
    val (lines, adjustedPos) = replaceTabs(trimmedLines, trimmedPos)

    if (formatting.useColor)
      lines.map { case (lineNum, line) => coloredIndicatorLine(lineNum, line, adjustedPos) }
    else
      lines.flatMap { case (lineNum, line) => indicatorLines(lineNum, line, adjustedPos) }
  }


  private def contextLines: List[(Int, String)] = {
    val start = (position.line - messageContextSize).clamp(1, lines.size)
    val end = (position.line + messageContextSize).clamp(1, lines.size)
    (start to end)
      .map(i => (i, lines(i - 1)))
      .toList
  }

  private def trimIndent(lines: List[(Int, String)]): (List[(Int, String)], Position) = {
    val indent = getMinimumIndent(lines)

    val trimmedLines = lines.map { case (lineNum, line) =>
      val trimmedLine = if (line.nonEmpty) line.substring(indent) else ""
      (lineNum, trimmedLine)
    }
    val start = math.max(1, position.col - indent)
    val end = math.max(1, position.endCol - indent)
    val adjustedPos = Position(position.line, start, position.endLine, end)
    (trimmedLines, adjustedPos)
  }


  private def replaceTabs(lines: List[(Int, String)], pos: Position): (List[(Int, String)], Position) = {
    var start = pos.col
    var end = pos.endCol
    val tabReplacedLines = lines map { case (lineNum, line) =>
      var sb = new StringBuilder
      for (i <- 0 until line.length) {
        val c = line(i)
        c match {
          case '\t' =>
            if (lineNum == pos.line && start - 1 > i) start += tabWidth - 1
            if (lineNum == pos.endLine && end - 1 >= i) end += tabWidth - 1
            sb.appendTimes(' ', tabWidth)
          case _    =>
            sb += c
        }
      }
      (lineNum, sb.toString)
    }
    val adjustedPos = Position(pos.line, start, pos.endLine, end)
    (tabReplacedLines, adjustedPos)
  }

  private def getMinimumIndent(lines: List[(Int, String)]): Int =
    lines
      .filter { case (_, line) => line.nonEmpty }
      .map { case (_, line) => indent(line) }.min

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
    import formatting._
    val highlightedLine = if (lineNum in (pos.line to pos.endLine)) {
      val MarkColor = Underline + color
      val (start, end) = startAndEnd(line, lineNum, pos)
      line.substring(0, start) + MarkColor(line.substring(start, end)) + line.substring(end, line.length)
    } else {
      line
    }

    (NumColor(lineNum), formatter.syntaxHighlight(highlightedLine))
  }

  private def indicatorLines(lineNum: Int, line: String, pos: Positioned): List[(String, String)] = {
    val firstLine = (lineNum.toString, line)
    if (lineNum notIn (pos.line to pos.endLine))
      return firstLine :: Nil

    val (start, end) = startAndEnd(line, lineNum, pos)
    val indicator = formatting.UnderlineCharacter * (end - start)
    val indentation = " " * start
    firstLine :: ("", indentation + indicator) :: Nil
  }

  private def startAndEnd(line: String, lineNum: Int, pos: Positioned): (Int, Int) = {
    val startOfText = line.indexWhere(!_.isWhitespace)
    val start = if (lineNum == pos.line) pos.col - 1 else 0
    val end = if (lineNum == pos.endLine) pos.endCol - 1 else line.length
    (Math.max(startOfText, start), Math.min(end, line.length))
  }


}
