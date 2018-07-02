package tlang.compiler.output

import tlang.compiler.messages._
import tlang.formatting.Formatter
import tlang.formatting.grid.{CenteredContent, Column, Grid, TruncatedColumn}
import tlang.formatting.textformatters.TabReplacer
import tlang.options.argument.MessageContextFlag
import tlang.utils.Extensions._

object ErrorMessageOutput {
  val DefaultMessageTypes = List(MessageType.Warning, MessageType.Error)
}

case class ErrorMessageOutput(
  compilerMessages: CompilerMessages,
  tabReplacer: TabReplacer = TabReplacer(2),
  messageContextSize: Int = MessageContextFlag.defaultValue,
  messageTypes: List[MessageType] = ErrorMessageOutput.DefaultMessageTypes
) extends Output {

  override def pretty(formatter: Formatter): String = {
    val formatting = formatter.formatting
    import formatting._

    def format(messageType: MessageType): String = {
      val messages = compilerMessages(messageType)
      if (messages.isEmpty)
        return ""
      val grid = formatter.grid.header(header(messageType))
      messages.foreach { addToGrid(grid, _) }
      grid.render()
    }

    def header(messageType: MessageType): String = {
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

    def addToGrid(grid: Grid, message: CompilerMessage): Unit = {
      addMessage(message, grid, showTitle = true)
      message.extraInfo foreach { addMessage(_, grid, showTitle = false) }
    }

    def addMessage(message: CompilerMessage, grid: Grid, showTitle: Boolean): Unit = {
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

    messageTypes.map(format).filter(_.nonEmpty).mkString(NL)
  }


  override def json(): Map[String, Any] = {

    def format(messageType: MessageType): Map[String, Any] = {
      val messages = compilerMessages(messageType)
      val key = s"compilation${messageType}s"

      Map(key -> messages.map(formatMessage))
    }

    def formatMessage(message: CompilerMessage): Map[String, Any] = {
      val pos = message.pos
      Map(
        "file" -> pos.sourceName,
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

    messageTypes.map(format).reduce(_ ++ _)
  }
}

