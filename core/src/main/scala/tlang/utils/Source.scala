package tlang.utils


import java.nio.file.Path

import better.files.File
import tlang.Constants
import tlang.formatting.Formatting

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
    relativize(file.parent.path) + file.fileSystem.getSeparator + fileName
  }


  private def relativize(path: Path): Path = {
    val absolute = path.toAbsolutePath
    if (absolute.startsWith(Constants.Pwd))
      Constants.Pwd.relativize(absolute)
    else
      absolute
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