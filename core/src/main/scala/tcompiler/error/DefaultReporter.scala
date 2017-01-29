package tcompiler.error

import tcompiler.Flags
import tcompiler.analyzer.Symbols.Symbolic
import tcompiler.analyzer.Types.Typed
import tcompiler.utils.Colorizer

import scala.collection.mutable

class CompilationException(message: String) extends Exception(message)

trait Reporter {

  def report(error: Error): Unit
  def clear(): Unit
  def terminateIfErrors(): Unit

  def hasErrors: Boolean
  def hasWarnings: Boolean

  def errorsString: String
  def warningsString: String

}

class VoidReporter extends Reporter {
  override def report(error: Error): Unit = {}
  override def clear(): Unit = {}
  override def terminateIfErrors(): Unit = {}

  override def hasErrors: Boolean = false
  override def hasWarnings: Boolean = false
  override def errorsString: String = ""
  override def warningsString: String = ""
}


class DefaultReporter(
  suppressWarnings: Boolean = false,
  warningIsError: Boolean = false,
  colorizer: Colorizer = new Colorizer(false),
  maxErrors: Int = Flags.MaxErrors.Default,
  errorContext: Int = Flags.ErrorContext.Default
) extends Reporter {

  import colorizer._

  private var hitMaxErrors = false

  val errors  : mutable.LinkedHashSet[Error] = mutable.LinkedHashSet()
  val warnings: mutable.LinkedHashSet[Error] = mutable.LinkedHashSet()


  def report(error: Error): Unit = {
    error.errorLevel match {
      case ErrorLevel.Warning =>
        if (warningIsError) {
          report(error.copy(errorLevel = ErrorLevel.Error))
          return
        }

        if (suppressWarnings || warnings.contains(error))
          return

        warnings += error
      case ErrorLevel.Error   =>
        if (!isValidError(error))
          return

        if (maxErrors != -1 && errors.size >= maxErrors) {
          hitMaxErrors = true
          return
        }
        if (errors.contains(error))
          return

        errors += error
      case ErrorLevel.Fatal   =>
        errors += error
        val errorFormatter = ErrorFormatter(error, colorizer, errorContext)
        throw new CompilationException(errorFormatter.format())
    }
  }

  def clear(): Unit = {
    errors.clear()
    warnings.clear()
    hitMaxErrors = false
  }

  def terminateIfErrors(): Unit =
    if (hasErrors)
      throw new CompilationException(errorsString)

  def hasErrors: Boolean = errors.nonEmpty
  def hasWarnings: Boolean = warnings.nonEmpty

  def errorsString: String = {
    val err = errorString(errors)

    val numErrors = errors.size
    val num = s"$Red$Bold$numErrors$Reset"

    val prefix = if (hitMaxErrors)
      s"There were more than $num errors, only showing the first $num"
    else if (numErrors == 1)
      s"There was $num error"
    else
      s"There were $num errors"

    prefix + s":\n\n$err"
  }


  def warningsString: String = errorString(warnings)


  private def errorString(errors: mutable.LinkedHashSet[Error]) =
    errors
      .map { err => ErrorFormatter(err, colorizer, errorContext).format() }
      .mkString("\n")

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

