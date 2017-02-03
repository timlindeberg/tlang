package tcompiler.error

import tcompiler.Flags
import tcompiler.analyzer.Symbols.Symbolic
import tcompiler.analyzer.Types.Typed

import scala.collection.mutable

class CompilationException(message: String) extends Exception(message)

trait Reporter {

  def report(error: Error): Unit
  def clear(): Unit
  def terminateIfErrors(): Unit

  def hasErrors: Boolean
  def hasWarnings: Boolean

  def errorMessage: String
  def warningMessage: String

}

class VoidReporter extends Reporter {
  override def report(error: Error): Unit = {}
  override def clear(): Unit = {}
  override def terminateIfErrors(): Unit = {}

  override def hasErrors: Boolean = false
  override def hasWarnings: Boolean = false
  override def errorMessage: String = ""
  override def warningMessage: String = ""
}


class DefaultReporter(
  suppressWarnings: Boolean = false,
  warningIsError: Boolean = false,
  formatting: Formatting = SimpleFormatting,
  maxErrors: Int = Flags.MaxErrors.defaultValue,
  errorContext: Int = Flags.ErrorContext.defaultValue
) extends Reporter {

  private var hitMaxErrors = false

  val errors  : mutable.LinkedHashSet[Error] = mutable.LinkedHashSet()
  val warnings: mutable.LinkedHashSet[Error] = mutable.LinkedHashSet()

  import formatting.colors._


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
        val errorFormatter = ErrorFormatter(error, formatting, errorContext)
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
      throw new CompilationException(errorMessage)

  def hasErrors: Boolean = errors.nonEmpty
  def hasWarnings: Boolean = warnings.nonEmpty

  def errorMessage: String = {

    val numErrors = errors.size
    val num = Red(numErrors)

    val header = if (hitMaxErrors)
      s"${Bold}There were more than $num$Bold errors, only showing the first $num$Reset"
    else
      getPrefix(errors, "error", num)

    formatting.makeBox(header, Nil) + format(errors)
  }


  def warningMessage: String = {
    val numWarnings = warnings.size
    val num = Yellow(numWarnings)
    val header = getPrefix(warnings, "warning", num)
    formatting.makeBox(header, Nil) + format(warnings)
  }

  private def getPrefix(errors: mutable.LinkedHashSet[Error], tpe: String, num: String) = {
    val n = errors.size
    val was = if (n == 1) "was" else "were"
    s"${Bold}There $was $num$Bold $tpe" + (if (n > 1) "s" else "") + Reset
  }

  private def format(errors: mutable.LinkedHashSet[Error]) =
    errors
      .map { err => ErrorFormatter(err, formatting, errorContext).format() }
      .mkString

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

