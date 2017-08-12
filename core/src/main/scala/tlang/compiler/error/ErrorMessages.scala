package tlang.compiler.error

import tlang.utils.Extensions._
import tlang.utils.FileSource
import tlang.utils.formatting.Colors.Color
import tlang.utils.formatting.Formatting
import tlang.utils.formatting.grid.Grid

import scala.collection.mutable

case class ErrorMessages(formatting: Formatting, maxErrors: Int, errorContext: Int) {

  import formatting._

  private var hitMaxWarnings = false
  private var hitMaxErrors   = false
  val warnings: mutable.Set[Warning]      = mutable.LinkedHashSet()
  val errors  : mutable.Set[ErrorMessage] = mutable.LinkedHashSet()

  def getErrors: List[ErrorMessage] = errors.toList
  def getWarnings: List[Warning] = warnings.toList

  def +=(error: ErrorMessage): ErrorMessages = {
    error match {
      case warning: Warning =>
        if (maxErrors != -1 && warnings.size >= maxErrors) {
          hitMaxWarnings = true
          return this
        }
        warnings += warning
      case _                =>
        if (maxErrors != -1 && errors.size >= maxErrors) {
          hitMaxErrors = true
          return this
        }
        errors += error
    }
    this
  }

  def clear(): Unit = {
    warnings.clear()
    errors.clear()
    hitMaxErrors = false
    hitMaxWarnings = false
  }

  def printWarnings(): Unit = {
    if (warnings.isEmpty)
      return

    println(formattedWarnings)
  }

  def printErrors(): Unit = {
    if (errors.isEmpty)
      return

    println(formattedErrors)
  }

  def formattedWarnings: String = {
    val grid = Grid(formatting).header(warningHeader)

    warnings.foreach { addToGrid(grid, _) }
    grid.toString
  }

  def formattedErrors: String = {
    val grid = Grid(formatting).header(errorHeader)

    errors.foreach { addToGrid(grid, _) }
    grid.toString
  }

  private def addToGrid(grid: Grid, error: ErrorMessage): Unit = {
    val errorFormatter = ErrorFormatter(error, formatting, errorContext)

    grid.row()

    val pos = error.pos
    val validPosition = error.pos.hasSource && (pos.line in (1 to errorFormatter.lines.size))

    if (validPosition && error.pos.source.isInstanceOf[FileSource])
      grid.content(errorFormatter.sourceDescription)

    grid.content(errorFormatter.errorPrefix + error.message)

    if (validPosition) {
      grid.row(2)
      grid.mapContent(errorFormatter.locationInFile) { x => x }
    }
  }

  private def warningHeader: String = header("warning", Yellow, hitMaxWarnings, warnings)
  private def errorHeader: String = header("error", Red, hitMaxErrors, errors)

  private def header(messageType: String, color: Color, hitMax: Boolean, messages: Iterable[ErrorMessage]): String = {
    val n = messages.size
    val (was, appendix) = if (n == 1) ("was", "") else ("were", "s")

    val name = messageType + appendix
    val num = color(n)
    if (hitMax)
      s"${ Bold }There were more than $num$Bold $name, only showing the first $num$Reset."
    else
      s"${ Bold }There $was $num$Bold $name.$Reset"
  }
}
