package tlang.utils

import better.files.File
import tlang.Constants
import tlang.formatting.Colors.Color
import tlang.formatting.Formatter
import tlang.utils.Extensions._

import scala.collection.mutable
import scala.io.StdIn

trait Source {
  def mainName: String
  def text: String
  def lines: IndexedSeq[String] = text.lines.toIndexedSeq
  def description(implicit formatter: Formatter): String = getDescription(formatter.NumColor)
  def errorDescription(implicit formatter: Formatter): String = getDescription(formatter.Red)

  def getDescription(color: Color)(implicit formatter: Formatter): String
}

object FileSource {

  private val TextCache: mutable.Map[File, String] = mutable.Map()

  def apply(path: String): FileSource = apply(File(path))

  def getText(file: File): String = TextCache.getOrElseUpdate(file, file.contentAsString)
  def clearCache(file: File): Unit = TextCache.remove(file)
  def clearCache(): Unit = TextCache.clear()
}

case class FileSource(file: File) extends Source {

  override def mainName: String = file.name.dropRight(Constants.FileEnding.length)
  override def text: String = FileSource.getText(file)
  override def getDescription(color: Color)(implicit formatter: Formatter): String = {
    import formatter._
    val style = Bold + color
    val fileName = style(file.name)
    file.parent.path.relativePWD + file.fileSystem.getSeparator + fileName
  }
}

case class StdinSource() extends Source {


  override def mainName: String = "stdin"
  override val text: String = readStdin()
  override def getDescription(color: Color)(implicit formatter: Formatter): String = {
    import formatter._
    val style = Bold + color
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
  override def getDescription(color: Color)(implicit formatter: Formatter): String = {
    import formatter._
    val style = Bold + color
    style(mainName)
  }
}
