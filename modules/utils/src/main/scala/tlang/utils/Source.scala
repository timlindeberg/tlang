package tlang.utils

import better.files.File
import tlang.Constants
import tlang.formatting.Formatting
import tlang.utils.Extensions._

import scala.collection.mutable

trait Source {
  def mainName: String
  def text: String
  def lines: IndexedSeq[String]
  def description(formatting: Formatting): String
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
  override def lines: IndexedSeq[String] = text.lines.toIndexedSeq
  override def description(formatting: Formatting): String = {
    import formatting._
    val style = Bold + NumColor
    val fileName = style(file.name)
    file.parent.path.relativePWD + file.fileSystem.getSeparator + fileName
  }
}

case class StringSource(str: String, override val mainName: String) extends Source {

  override def text: String = str
  override def lines: IndexedSeq[String] = text.lines.toIndexedSeq
  override def description(formatting: Formatting): String = {
    import formatting._
    val style = Bold + NumColor
    style(mainName)
  }

}