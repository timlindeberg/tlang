package tlang.compiler.error

import tlang.compiler.analyzer.Symbols.Symbolic
import tlang.compiler.analyzer.Types.Typed
import tlang.compiler.options.Flags

trait Reporter {

  def messages: ErrorMessages
  def report(error: ErrorMessage): Unit
  def clear(): Unit
  def terminateIfErrors(): Unit

  def hasErrors: Boolean
  def hasWarnings: Boolean
}

case class VoidReporter() extends Reporter {

  val messages: ErrorMessages = ErrorMessages(SimpleFormatting, -1, 2)
  private var _hasErrors   = false
  private var _hasWarnings = false

  override def report(error: ErrorMessage): Unit = error match {
    case _: Fatal   => throw new CompilationException(messages += error)
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
  suppressWarnings: Boolean = false,
  warningIsError: Boolean = false,
  formatting: Formatting = SimpleFormatting,
  maxErrors: Int = Flags.MaxErrors.defaultValue,
  errorContext: Int = Flags.ErrorContext.defaultValue
) extends Reporter {

  var messages: ErrorMessages = ErrorMessages(formatting, maxErrors, errorContext)

  def report(error: ErrorMessage): Unit = {
    error match {
      case _: Fatal   =>
        messages += error
        throwException()
      case w: Warning =>
        if (warningIsError) messages.errors += w else messages.warnings += w
      case _          =>
    }

    if (!isValidError(error))
      return

    messages += error
  }

  def clear(): Unit = messages = ErrorMessages(formatting, maxErrors, errorContext)

  def terminateIfErrors(): Unit =
    if (hasErrors)
      throwException()

  def hasErrors: Boolean = messages.errors.nonEmpty
  def hasWarnings: Boolean = messages.warnings.nonEmpty

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

