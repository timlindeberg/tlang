package tlang.utils

import java.io.File

import tlang.compiler.error.Formatting
import tlang.compiler.main.Main
import tlang.utils.Extensions._

import scala.collection.mutable

/**
  * Created by Tim Lindeberg on 2/18/2017.
  */

trait Source {
  def mainName: String
  def description(formatting: Formatting): String
  def text: String
}

object Source {

  private val TextCache: mutable.Map[File, String] = mutable.Map()
  def getText(file: File): String =
    TextCache.getOrElseUpdate(file, using(io.Source.fromFile(file)) {_.mkString})
}

case class FileSource(file: File) extends Source {

  override def mainName: String = file.getName.dropRight(Main.FileEnding.length)
  override def text: String = Source.getText(file)
  override def description(formatting: Formatting): String = {
    import formatting.colors._
    val FileNameStyle = Bold + NumColor
    val fileName = FileNameStyle(file.getName)
    file.getParent + File.separator + fileName + " | "
  }

}

case class StringSource(str: String, override val mainName: String) extends Source {
  override def text: String = str
  override def description(formatting: Formatting) = ""
}