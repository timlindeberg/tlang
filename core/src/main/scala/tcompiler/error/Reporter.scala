package tcompiler.error

import tcompiler.analyzer.Symbols.Symbolic
import tcompiler.analyzer.Types.Typed
import tcompiler.utils.Colored

import scala.collection.mutable

class CompilationException(message: String) extends Exception(message)

class Reporter(suppressWarnings: Boolean = false,
  warningIsError: Boolean = false,
  override val useColor: Boolean = false,
  maxErrors: Int = 100,
  errorContext: Int = 1)
  extends Colored {


  private val ErrorSeparator = "\n"

  private var hitMaxErrors = false

  var errors  : mutable.LinkedHashSet[Error] = mutable.LinkedHashSet()
  var warnings: mutable.LinkedHashSet[Error] = mutable.LinkedHashSet()


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
        val errorFormatter = ErrorFormatter(error, useColor, errorContext)
        throw new CompilationException(errorFormatter.format())
    }
  }

  def clear(): Unit = {
    errors.clear()
    warnings.clear()
    hitMaxErrors = false
  }

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

  def terminateIfErrors(): Unit =
    if (hasErrors)
      throw new CompilationException(errorsString)

  private def errorString(errors: mutable.LinkedHashSet[Error]) =
    errors
      .map { err => ErrorFormatter(err, useColor, errorContext).format() }
      .mkString(ErrorSeparator)

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

