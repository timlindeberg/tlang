package tlang.utils

import java.io.File

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
  def getText(file: File): String =
    TextCache.getOrElseUpdate(file, using(io.Source.fromFile(file)) { _.mkString })
}

object FileSource {
  def apply(path: String): FileSource = apply(new File(path))
}

case class FileSource(file: File) extends Source {

  override def mainName: String = file.getName.dropRight(Constants.FileEnding.length)
  override def text: String = Source.getText(file)
  override def lines: IndexedSeq[String] = text.lines.toIndexedSeq
  override def description(formatting: Formatting): String = {
    import formatting._
    val style = Bold + NumColor
    val fileName = style(file.getName)
    file.getParent + File.separator + fileName
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