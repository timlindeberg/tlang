package tlang.utils

import java.io.File

import tlang.Constants
import tlang.utils.Extensions._

import scala.collection.mutable

trait Source {
  def mainName: String
  def text: String
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

}

case class StringSource(str: String, override val mainName: String) extends Source {
  override def text: String = str
}