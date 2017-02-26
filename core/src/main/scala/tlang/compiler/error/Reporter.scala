package tlang.compiler.error

import tlang.compiler.analyzer.Symbols.Symbolic
import tlang.compiler.analyzer.Types.Typed
import tlang.compiler.options.Flags

trait Reporter {

  def messages: ErrorMessages
  def report(error: Error): Unit
  def clear(): Unit
  def terminateIfErrors(): Unit

  def hasErrors: Boolean
  def hasWarnings: Boolean
}

case class VoidReporter() extends Reporter {

  val messages: ErrorMessages = ErrorMessages(SimpleFormatting, -1, 2)
  private var _hasErrors   = false
  private var _hasWarnings = false

  override def report(error: Error): Unit = error.errorLevel match {
    case ErrorLevel.Warning => _hasWarnings = true
    case _                  => _hasErrors = true
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

  import ErrorLevel._

  var messages: ErrorMessages = ErrorMessages(formatting, maxErrors, errorContext)

  def report(error: Error): Unit = {
    val errorLevel = error.errorLevel
    if (errorLevel == Fatal) {
      messages += error
      throwException()
    }

    if (errorLevel == Warning && warningIsError) {
      report(error.copy(errorLevel = Error))
      return
    }

    if (!isValidError(error) || messages.contains(error))
      return

    messages += error
  }

  def clear(): Unit = messages = ErrorMessages(formatting, maxErrors, errorContext)

  def terminateIfErrors(): Unit =
    if (hasErrors)
      throwException()

  def hasErrors: Boolean = messages(ErrorLevel.Error).nonEmpty
  def hasWarnings: Boolean = messages(ErrorLevel.Warning).nonEmpty

  private def throwException() = {
    val exception = new CompilationException(messages)
    clear()
    throw exception
  }

  private def isValidError(error: Error): Boolean = {
    if (error.msg.toString.contains(Errors.ErrorName))
      return false

    error.pos match {
      case t: Typed if t.getType.name == Errors.ErrorName                        => false
      case s: Symbolic[_] if s.hasSymbol && s.getSymbol.name == Errors.ErrorName => false
      case _                                                                     => true
    }
  }
}

