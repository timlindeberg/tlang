package tlang.utils

import better.files.File
import tlang.Constants
import tlang.formatting.{Formatting, SimpleFormatting}
import tlang.utils.Extensions._

import scala.collection.mutable
import scala.io.StdIn

trait Source {
  def mainName: String
  def text: String
  def lines: IndexedSeq[String] = text.lines.toIndexedSeq
  def description(formatting: Formatting, isError: Boolean = false): String
  def description: String = description(SimpleFormatting, isError = false)
}

object Source {

  private val TextCache: mutable.Map[File, String] = mutable.Map()
  def getText(file: File): String = TextCache.getOrElseUpdate(file, file.contentAsString)
  def clearCache(file: File): Unit = TextCache.remove(file)
  def clearCache(): Unit = TextCache.clear()

}

object FileSource {
  def apply(path: String): FileSource = apply(File(path))
}

case class FileSource(file: File) extends Source {

  override def mainName: String = file.name.dropRight(Constants.FileEnding.length)
  override def text: String = Source.getText(file)
  override def description(formatting: Formatting, isError: Boolean = false): String = {
    import formatting._
    val style = Bold + (if(isError) Red else NumColor)
    val fileName = style(file.name)
    file.parent.path.relativePWD + file.fileSystem.getSeparator + fileName
  }
}

case class StdinSource() extends Source {


  override def mainName: String = "stdin"
  override val text: String = readStdin()
  override def description(formatting: Formatting, isError: Boolean): String = {
    import formatting._
    val style = Bold + (if(isError) Red else NumColor)
    style(mainName)
  }

  private def readStdin(): String = {
    var line = ""
    val sb = new StringBuilder
    while ({line = StdIn.readLine(); line != null}) {
      sb ++= line
      sb ++= NL
    }
    sb.toString()
  }
}

case class StringSource(str: String, override val mainName: String) extends Source {

  override def text: String = str
  override def description(formatting: Formatting, isError: Boolean = false): String = {
    import formatting._
    val style = Bold + (if(isError) Red else NumColor)
    style(mainName)
  }
}
