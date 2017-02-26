package tlang.compiler.error

import scala.collection.mutable

/**
  * Created by Tim Lindeberg on 2/26/2017.
  */
case class ErrorMessages(formatting: Formatting, maxErrors: Int, errorContext: Int) {

  import ErrorLevel._

  private val hitMax: mutable.Set[ErrorLevel] = mutable.Set()

  private val messages: Map[ErrorLevel, mutable.LinkedHashSet[Error]] = Map(
    Warning -> mutable.LinkedHashSet[Error](),
    Error -> mutable.LinkedHashSet[Error]()
  )

  def +=(error: Error): ErrorMessages = {
    val errorLevel = if (error.errorLevel == Fatal) Error else error.errorLevel

    if (maxErrors != -1 && messages(errorLevel).size >= maxErrors) {
      hitMax.add(errorLevel)
      return this
    }
    messages(errorLevel) += error
    this
  }

  def clear(): Unit = {
    messages.values.foreach(_.clear())
    hitMax.clear()
  }
  def apply(errorLevel: ErrorLevel): List[Error] = messages(errorLevel).toList
  def contains(error: Error): Boolean = messages(error.errorLevel).contains(error)

  def formattedMessage(errorLevels: ErrorLevel*): String = {
    val sb = new StringBuilder
    val warnings = formatMessages(messages(Warning))
    if (warnings.nonEmpty && errorLevels.contains(Warning)) {
      sb ++= formatHeader(Warning)
      sb ++= warnings
    }

    if (errorLevels.contains(Error)) {
      sb ++= formatHeader(Error)
      sb ++= formatMessages(messages(Error))
    }
    sb.toString()
  }

  private def formatMessages(messages: mutable.LinkedHashSet[Error]) =
    messages
      .map {ErrorFormatter(_, formatting, errorContext).format()}
      .mkString

  private def formatHeader(errorLevel: ErrorLevel) = {
    import formatting.colors._

    val n = messages(errorLevel).size
    val (was, appendix) = if (n == 1) ("was", "") else ("were", "s")

    val (color, name) = errorLevel match {
      case ErrorLevel.Error   => (Red, "error" + appendix)
      case ErrorLevel.Warning => (Yellow, "warning" + appendix)
    }


    val num = color(n)
    if (hitMax(errorLevel))
      s"${Bold}There were more than $num$Bold $name, only showing the first $num$Reset."
    else
      s"${Bold}There $was $num$Bold $name.$Reset"
  }


}
