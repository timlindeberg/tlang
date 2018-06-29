package tlang.compiler.messages

import tlang.formatting.Colors.Color
import tlang.formatting.Formatter
import tlang.formatting.grid.{CenteredContent, Column, Grid, TruncatedColumn}
import tlang.formatting.textformatters.TabReplacer
import tlang.options.argument.MessageContextFlag
import tlang.utils.Extensions._
import tlang.utils.{JSON, Position, Positioned}

trait MessageFormatter {
  def print(messages: CompilerMessages): Unit =
    printMessages(apply(messages))
  def print(messages: CompilerMessages, messageType: MessageType): Unit =
    printMessages(apply(messages, messageType))

  def apply(messages: CompilerMessages): String
  def apply(messages: CompilerMessages, messageType: MessageType): String

  private def printMessages(s: String): Unit = {
    if(s.nonEmpty)
      println(s)
  }
}

case class JSONMessageFormatter() extends MessageFormatter {
  override def apply(messages: CompilerMessages): String = {
    JSON(formatMessages(messages, MessageType.Warning) ++ formatMessages(messages, MessageType.Error))
  }

  override def apply(messages: CompilerMessages, messageType: MessageType): String = {
    JSON(formatMessages(messages, messageType))
  }

  private def formatMessages(compilerMessages: CompilerMessages, messageType: MessageType): Map[String, Any] = {
    val messages = compilerMessages(messageType)
    val key = messageType.toString.toLowerCase + "s"

    Map(key -> messages.map(formatMessage))
  }

  private def formatMessage(message: CompilerMessage): Map[String, Any] = {
    val pos = message.pos
    Map(
      "start" -> Map(
        "line" -> pos.line,
        "col" -> pos.col
      ),
      "end" -> Map(
        "line" -> pos.lineEnd,
        "col" -> pos.colEnd
      ),
      "code" -> message.code,
      "message" -> message.message
    )
  }
}

case class PrettyMessageFormatter(
  formatter: Formatter,
  tabReplacer: TabReplacer,
  messageContextSize: Int = MessageContextFlag.defaultValue) extends MessageFormatter {

  private val formatting = formatter.formatting

  import formatting._

  override def apply(messages: CompilerMessages): String = {
    apply(messages, MessageType.Error) + NL + apply(messages, MessageType.Warning)
  }

  override def apply(compilerMessages: CompilerMessages, messageType: MessageType): String = {
    val messages = compilerMessages(messageType)
    if (messages.isEmpty)
      return ""
    val grid = formatter.grid.header(header(compilerMessages, messageType))
    messages.foreach { addToGrid(grid, _) }
    grid.render()
  }

  private def header(compilerMessages: CompilerMessages, messageType: MessageType): String = {
    val messages = compilerMessages(messageType)

    val n = messages.size
    if (n == 0)
      return ""

    val (was, appendix) = if (n == 1) ("was", "") else ("were", "s")

    val name = messageType.name.toLowerCase + appendix
    val color = messageType.color(formatter.formatting)
    val num = color(n)
    if (compilerMessages.hitMax(messageType))
      s"${ Bold }There were more than $num$Bold $name, only showing the first $num$Reset."
    else
      s"${ Bold }There $was $num$Bold $name.$Reset"
  }

  private def addToGrid(grid: Grid, message: CompilerMessage): Unit = {
    addMessage(message, grid, showTitle = true)
    message.extraInfo.foreach { addMessage(_, grid, showTitle = false) }
  }

  private def addMessage(message: CompilerMessage, grid: Grid, showTitle: Boolean): Unit = {
    val formatting = formatter.formatting
    val messageInfo = MessageInfo(message, formatter, tabReplacer, messageContextSize)

    grid.row()

    if (showTitle) {
      val color = message.messageType.color(formatting)
      val title = s" ${ messageInfo.prefix.stripAnsi } "
      grid.content(CenteredContent(title, color, formatting.HorizontalThick))
    }

    val validPos = messageInfo.hasValidPosition
    if (validPos)
      grid.content(messageInfo.sourceDescription)

    grid.content(message.message)

    if (validPos) {
      grid.row(Column, TruncatedColumn)
      grid.contents(messageInfo.locationInSource)
    }
  }

}

case class MessageInfo(message: CompilerMessage, formatter: Formatter, tabReplacer: TabReplacer, messageContextSize: Int = MessageContextFlag.defaultValue) {

  private val lines: IndexedSeq[String] = message.pos.source.map(_.lines).getOrElse(IndexedSeq())
  private val formatting = formatter.formatting

  import formatting._

  def color: Color = message.messageType.color(formatting) + formatting.Bold
  def position: Positioned = message.pos

  def hasValidPosition: Boolean = position.source.nonEmpty && (position.line in (1 to lines.size + 1))

  def prefix: String = color(message.messageType.name + " " + message.code)

  def positionDescription: String = {
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

  def locationInSource: Seq[(String, String)] = {

    val (trimmedLines, trimmedPos) = trimIndent(contextLines)
    val (lines, adjustedPos) = tabReplacer(trimmedLines, trimmedPos)

    if (formatting.useColor)
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

    (NumColor(lineNum), formatter.syntaxHighlight(highlightedLine))
  }

  private def indicatorLines(lineNum: Int, line: String, pos: Positioned): List[(String, String)] = {
    val firstLine = (lineNum.toString, line)
    if (lineNum notIn (pos.line to pos.lineEnd))
      return firstLine :: Nil

    val (start, end) = startAndEnd(line, lineNum, pos)
    val indicator = formatting.UnderlineCharacter * (end - start)
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
