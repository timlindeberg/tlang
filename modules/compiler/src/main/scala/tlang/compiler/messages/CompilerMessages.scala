package tlang.compiler.messages

import tlang.compiler.analyzer.Symbols.{ClassErrorSymbol, Symbolic, VariableErrorSymbol}
import tlang.compiler.analyzer.Types.{TError, Typed}
import tlang.formatting.Formatter
import tlang.formatting.grid.{Column, Grid, TruncatedColumn}
import tlang.options.argument.MaxErrorsFlag

import scala.collection.mutable

object CompilerMessages {
  def createMessages(): mutable.Map[MessageType, mutable.Set[CompilerMessage]] = mutable.Map(
    MessageType.Error -> mutable.LinkedHashSet[CompilerMessage](),
    MessageType.Warning -> mutable.LinkedHashSet[CompilerMessage]()
  )
}

case class CompilerMessages(
  formatter: Formatter,
  messageFormatter: MessageFormatter,
  maxErrors: Int = MaxErrorsFlag.defaultValue,
  warningIsError: Boolean = false,
  suppressWarnings: Boolean = false,
  private var messages: mutable.Map[MessageType, mutable.Set[CompilerMessage]] = CompilerMessages.createMessages()) {

  import formatter.formatting._

  private val hitMax = mutable.Set[MessageType]()

  override def clone(): CompilerMessages = copy(messages = mutable.Map() ++ messages.toMap)

  def +=(message: CompilerMessage): CompilerMessages = {
    if (!isValidError(message))
      return this

    var messageType = message.messageType
    if (messageType == MessageType.Warning) {
      if (suppressWarnings)
        return this

      // Copy the existing values such as code type etc., only change should be the message type
      if (warningIsError)
        return this += message.copy(messageType = MessageType.Error)
    }
    // The CompilerMessages class treats fatal as error
    if (messageType == MessageType.Fatal)
      messageType = MessageType.Error

    if (maxErrors >= 0 && messages(messageType).size >= maxErrors) {
      hitMax += messageType
      return this
    }
    messages(messageType) += message
    this
  }

  def apply(messageType: MessageType): List[CompilerMessage] = messages(messageType).toList

  def clear(): Unit = {
    messages = CompilerMessages.createMessages()
    hitMax.clear()
  }

  def print(): Unit = {
    print(MessageType.Warning)
    print(MessageType.Error)
  }

  def print(messageType: MessageType): Unit = {
    if (messages(messageType).isEmpty)
      return

    println(formatMessages(messageType))
  }

  def formatMessages(messageType: MessageType): String = {
    if (messages(messageType).isEmpty)
      return ""
    val grid = formatter.grid.header(header(messageType))
    messages(messageType).foreach { addToGrid(grid, _) }
    grid.render()
  }

  def header(messageType: MessageType): String = {
    val n = messages(messageType).size
    if (n == 0)
      return ""

    val (was, appendix) = if (n == 1) ("was", "") else ("were", "s")

    val name = messageType.name.toLowerCase + appendix
    val color = messageType.color(formatter.formatting)
    val num = color(n)
    if (hitMax(messageType))
      s"${ Bold }There were more than $num$Bold $name, only showing the first $num$Reset."
    else
      s"${ Bold }There $was $num$Bold $name.$Reset"
  }

  private def isValidError(error: CompilerMessage): Boolean =
    error.pos match {
      case t: Typed if t.getType == TError => false
      case s: Symbolic[_] if s.hasSymbol   =>
        val sym = s.getSymbol
        sym != ClassErrorSymbol && sym != VariableErrorSymbol
      case _                               => true
    }

  private def addToGrid(grid: Grid, error: CompilerMessage): Unit = {
    messageFormatter.setMessage(error)

    grid.row()

    val hasValidPosition = messageFormatter.hasValidPosition
    if (hasValidPosition)
      grid.content(messageFormatter.sourceDescription)

    grid.content(messageFormatter.prefix + " " + error.message)

    if (hasValidPosition) {
      grid.row(Column, TruncatedColumn)
      grid.contents(messageFormatter.locationInSource)
    }
  }


}
