package tlang.compiler.error

import tlang.compiler.analyzer.Symbols.Symbolic
import tlang.compiler.analyzer.Types.Typed
import tlang.compiler.options.Flags.MaxErrors
import tlang.utils.Extensions._
import tlang.utils.FileSource
import tlang.utils.formatting.grid.Grid

import scala.collection.mutable

case class CompilerMessages(
  errorFormatter: MessageFormatter,
  maxErrors: Int = MaxErrors.defaultValue,
  warningIsError: Boolean = false,
  suppressWarnings: Boolean = false) {

  private val formatter = errorFormatter.formatter

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


  private def isValidError(error: CompilerMessage): Boolean =
    error.pos match {
      case t: Typed if t.getType.name == CompilerMessage.ErrorName                        => false
      case s: Symbolic[_] if s.hasSymbol && s.getSymbol.name == CompilerMessage.ErrorName => false
      case _                                                                              => true
    }

  private def addToGrid(grid: Grid, error: CompilerMessage): Unit = {
    errorFormatter.setError(error)

    grid.row()

    val pos = error.pos
    val validPosition = error.pos.hasSource && (pos.line in (1 to errorFormatter.lines.size))

    if (validPosition && error.pos.source.isInstanceOf[FileSource])
      grid.content(errorFormatter.sourceDescription)

    grid.content(errorFormatter.prefix + error.message)

    if (validPosition) {
      grid.row(2)
      grid.contents(errorFormatter.locationInFile)
    }
  }

  private def header(messageType: MessageType): String = {
    val n = messages(messageType).size
    val (was, appendix) = if (n == 1) ("was", "") else ("were", "s")

    val name = messageType.name + appendix
    val num = messageType.color(formatter.formatting)
    if (hitMax(messageType))
      s"${ Bold }There were more than $num$Bold $name, only showing the first $num$Reset."
    else
      s"${ Bold }There $was $num$Bold $name.$Reset"
  }
}
