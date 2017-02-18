package tlang.compiler.error

import tlang.compiler.analyzer.Symbols.Symbolic
import tlang.compiler.analyzer.Types.Typed
import tlang.compiler.main.Flags

import scala.collection.mutable

trait Reporter {

  def report(error: Error): Unit
  def clear(): Unit
  def terminateIfErrors(): Unit

  def hasErrors: Boolean
  def hasWarnings: Boolean

  def errorMessage: String
  def warningMessage: String

}

case class VoidReporter() extends Reporter {

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
  override val errorMessage  : String = ""
  override val warningMessage: String = ""
}


case class DefaultReporter(
  suppressWarnings: Boolean = false,
  warningIsError: Boolean = false,
  formatting: Formatting = SimpleFormatting,
  maxErrors: Int = Flags.MaxErrors.defaultValue,
  errorContext: Int = Flags.ErrorContext.defaultValue
) extends Reporter {

  val hitMax: mutable.Set[ErrorLevel]                               = mutable.Set()
  val errors: mutable.Map[ErrorLevel, mutable.LinkedHashSet[Error]] = mutable.Map(
    ErrorLevel.Warning -> mutable.LinkedHashSet[Error](),
    ErrorLevel.Error -> mutable.LinkedHashSet[Error]()
  )

  import formatting.colors._


  def report(error: Error): Unit = {
    val errorLevel = error.errorLevel
    if (errorLevel == ErrorLevel.Fatal) {
      errors(ErrorLevel.Error) += error
      throw new CompilationException(errorMessage)
    }

    if (errorLevel == ErrorLevel.Warning && warningIsError) {
      report(error.copy(errorLevel = ErrorLevel.Error))
      return
    }
    if (maxErrors != -1 && errors(errorLevel).size >= maxErrors) {
      hitMax.add(errorLevel)
      return
    }

    if (!isValidError(error) || errors(errorLevel).contains(error))
      return

    errors(errorLevel) += error
  }

  def clear(): Unit = {
    errors.values.foreach(_.clear())
    hitMax.clear()
  }

  def terminateIfErrors(): Unit =
    if (hasErrors)
      throw new CompilationException(errorMessage)

  def hasErrors: Boolean = errors(ErrorLevel.Error).nonEmpty
  def hasWarnings: Boolean = errors(ErrorLevel.Warning).nonEmpty

  def errorMessage: String = format(ErrorLevel.Error)

  def warningMessage: String = format(ErrorLevel.Warning)

  private def format(errorLevel: ErrorLevel) = {
    val n = errors(errorLevel).size
    val (was, appendix) = if (n == 1) ("was", "s") else ("were", "")

    val (color, name) = errorLevel match {
      case ErrorLevel.Error   => (Red, "error" + appendix)
      case ErrorLevel.Warning => (Yellow, "warning" + appendix)
    }


    val num = color(n)
    val header = if (hitMax(errorLevel))
      s"${Bold}There were more than $num$Bold $name, only showing the first $num$Reset."
    else
      s"${Bold}There $was $num$Bold $name.$Reset"

    formatting.makeBox(header, Nil) +
      errors(errorLevel)
        .map {ErrorFormatter(_, formatting, errorContext).format()}
        .mkString
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

