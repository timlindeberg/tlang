package tlang.compiler.error

import java.io.File

import tlang.compiler.options.Flags
import tlang.utils.FileSource

import scala.collection.mutable

/**
  * Created by Tim Lindeberg on 2/26/2017.
  */
case class ErrorMessages(
  formatting: Formatting = SimpleFormatting,
  maxErrors: Int = Flags.MaxErrors.defaultValue,
  errorContext: Int = Flags.ErrorContext.defaultValue) {

  import formatting._
  import formatting.colors._

  private var hitMaxWarnings = false
  private var hitMaxErrors   = false
  val warnings: mutable.LinkedHashSet[Warning]      = mutable.LinkedHashSet()
  val errors  : mutable.LinkedHashSet[ErrorMessage] = mutable.LinkedHashSet()

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

  def formattedWarnings: String = {
    val sb = new StringBuilder
    val messages = formatMessages(warnings)
    if (messages.nonEmpty) {
      sb ++= warningHeader
      sb ++= messages
    }

    sb.toString()
  }

  def formattedErrors: String = {
    val sb = new StringBuilder
    sb ++= errorHeader
    sb ++= formatMessages(errors)
    sb.toString()
  }

  private def formatMessages(messages: mutable.LinkedHashSet[_ <: ErrorMessage]): String = {
    messages.map { error =>
      val errorFormatter = ErrorFormatter(error, formatting, errorContext)
      val sb = new StringBuilder

      sb ++= top

      val pos = error.pos
      val lines = errorFormatter.lines

      val validPosition = error.pos.hasSource && (1 to lines.size contains pos.line)

      if (validPosition && error.pos.source.isInstanceOf[FileSource]) {
        val file = error.pos.source.asInstanceOf[FileSource].file
        val fileNameStyle = Bold + NumColor
        val fileName = fileNameStyle(file.getName)
        val fileDescription = file.getParent + File.separator + fileName
        val sourceDescription = errorFormatter.position + " " + fileDescription
        sb ++= makeLines(sourceDescription)
      }

      sb ++= makeLines(errorFormatter.errorPrefix + error.message)


      if (validPosition)
        sb ++= makeBlocksWithColumns(errorFormatter.locationInFile, endOfBlock = true)
      else
        sb ++= bottom

      sb.toString()
    }.mkString
  }

  private def warningHeader = {
    val n = warnings.size
    val (was, appendix) = if (n == 1) ("was", "") else ("were", "s")

    val name = "warning" + appendix

    val num = Yellow(n)
    val text = if (hitMaxWarnings)
      s"${Bold}There were more than $num$Bold $name, only showing the first $num$Reset."
    else
      s"${Bold}There $was $num$Bold $name.$Reset"
    makeBox(text, Nil)
  }


  private def errorHeader = {
    val n = errors.size
    val (was, appendix) = if (n == 1) ("was", "") else ("were", "s")

    val name = "error" + appendix

    val num = Red(n)
    val text = if (hitMaxErrors)
      s"${Bold}There were more than $num$Bold $name, only showing the first $num$Reset."
    else
      s"${Bold}There $was $num$Bold $name.$Reset"
    makeBox(text, Nil)
  }

}
