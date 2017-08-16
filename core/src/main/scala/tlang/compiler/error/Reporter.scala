package tlang.compiler.error

import tlang.compiler.analyzer.Symbols.Symbolic
import tlang.compiler.analyzer.Types.Typed
import tlang.utils.formatting.{Formatting, SimpleFormatting}

trait Reporter {

  def printWarnings(): Unit
  def printErrors(): Unit
  def getWarnings: List[ErrorMessage]
  def getErrors: List[ErrorMessage]

  def report(error: ErrorMessage): Unit
  def clear(): Unit
  def terminateIfErrors(): Unit

  def hasErrors: Boolean
  def hasWarnings: Boolean
}

case class VoidReporter() extends Reporter {

  private var _hasErrors   = false
  private var _hasWarnings = false

  override def printWarnings(): Unit = {}
  override def printErrors(): Unit = {}
  override def getWarnings: List[ErrorMessage] = Nil
  override def getErrors: List[ErrorMessage] = Nil

  override def report(error: ErrorMessage): Unit = error match {
    case _: Fatal   => throw new CompilationException(null)
    case _: Warning => _hasWarnings = true
    case _          => _hasErrors = true
  }

  override def clear(): Unit = {
    _hasErrors = false
    _hasWarnings = false
  }
  override def terminateIfErrors(): Unit = {}

  override def hasErrors: Boolean = _hasErrors
  override def hasWarnings: Boolean = _hasWarnings

}


case class DefaultReporter(
  messages: ErrorMessages,
  formatting: Formatting = SimpleFormatting,
  suppressWarnings: Boolean = false,
  warningIsError: Boolean = false
) extends Reporter {

  def report(error: ErrorMessage): Unit = {
    error match {
      case _: Fatal   =>
        messages += error
        throwException()
      case w: Warning =>
        if (!suppressWarnings) {
          if (warningIsError) messages.errors += w else messages.warnings += w
        }
      case _          =>
    }

    if (!isValidError(error))
      return

    messages += error
  }

  def clear(): Unit = messages.clear()

  def terminateIfErrors(): Unit =
    if (hasErrors)
      throwException()

  def hasErrors: Boolean = messages.errors.nonEmpty
  def hasWarnings: Boolean = messages.warnings.nonEmpty

  override def printWarnings(): Unit = messages.printWarnings()
  override def printErrors(): Unit = messages.printErrors()
  override def getWarnings: List[ErrorMessage] = messages.getWarnings
  override def getErrors: List[ErrorMessage] = messages.getErrors

  private def throwException() = {
    val exception = new CompilationException(messages)
    clear()
    throw exception
  }

  private def isValidError(error: ErrorMessage): Boolean =
    error.pos match {
      case t: Typed if t.getType.name == ErrorMessage.ErrorName                        => false
      case s: Symbolic[_] if s.hasSymbol && s.getSymbol.name == ErrorMessage.ErrorName => false
      case _                                                                           => true
    }

}

