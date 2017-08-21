package tlang.compiler.error

import tlang.compiler.analyzer.Symbols.{ClassErrorSymbol, Symbolic, VariableErrorSymbol}
import tlang.compiler.analyzer.Types.{TError, Typed}
import tlang.compiler.options.Flags.MaxErrors
import tlang.formatting.Formatter
import tlang.formatting.grid.Grid

import scala.collection.mutable

case class CompilerMessages(
  formatter: Formatter,
  messageFormatter: MessageFormatter,
  maxErrors: Int = MaxErrors.defaultValue,
  warningIsError: Boolean = false,
  suppressWarnings: Boolean = false) {

  import formatter.formatting._

  private val hitMax                                                           = mutable.Set[MessageType]()
  private val messages: mutable.Map[MessageType, mutable.Set[CompilerMessage]] = mutable.Map(
    MessageType.Error -> mutable.LinkedHashSet[CompilerMessage](),
    MessageType.Warning -> mutable.LinkedHashSet[CompilerMessage]()
  )

  def +=(error: CompilerMessage): CompilerMessages = {
    if (!isValidError(error))
      return this

    var messageType = error.messageType
    if (messageType == MessageType.Warning) {
      if (suppressWarnings)
        return this
      if (warningIsError)
        messageType = MessageType.Error
    }
    // The CompilerMessages class treats fatal as error
    if (messageType == MessageType.Fatal)
      messageType = MessageType.Error

    if (maxErrors != -1 && messages(messageType).size >= maxErrors) {
      hitMax += messageType
      return this
    }
    messages(messageType) += error
    this
  }

  def apply(messageType: MessageType): List[CompilerMessage] = messages(messageType).toList

  def clear(): Unit = {
    messages.values.foreach(_.clear)
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
    val grid = Grid(formatter).header(header(messageType))
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
      grid.row(2)
      grid.contents(messageFormatter.locationInSource)
    }
  }


}
