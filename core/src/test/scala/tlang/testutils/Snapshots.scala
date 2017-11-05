package tlang.testutils

import better.files.File
import tlang.utils.Extensions._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object Snapshots {
  val SnapshotNamePrefix: String = "========================================= "
}

case class Snapshots(file: File) {

  import Snapshots._

  private      var isDirty  : Boolean                     = false
  private lazy val snapshots: mutable.Map[String, String] = parse

  def +=(keyValue: (String, String)): Unit = keyValue match {
    case (name, newSnapshot) =>
      snapshots.get(name) match {
        case Some(snapshot) if snapshot == newSnapshot => // Do nothing if the snapshot is the same
        case _                                         =>
          isDirty = true
          snapshots += keyValue
      }
  }

  def apply(name: String): Option[String] = snapshots.get(name)

  def save(): Unit = {
    if (!isDirty)
      return

    val content = snapshots
      .toSeq
      .sortBy { case (name, _) => name }
      .map { case (name, value) => SnapshotNamePrefix + name + NL + value }
      .mkString(NL + NL) + NL

    file.parent.createDirectories()
    file.write(content)
  }

  private def parse: mutable.Map[String, String] = {
    val map = mutable.Map[String, String]()

    if (!file.exists)
      return map

    val lines = ListBuffer[String]()
    var name = ""

    def addSnapshot(): Unit = {
      if (name.nonEmpty)
        map += name -> lines.dropRight(1).mkString(NL)
      lines.clear()
    }

    file.lineIterator foreach { line =>
      if (line.startsWith(SnapshotNamePrefix)) {
        addSnapshot()
        name = line.stripPrefix(SnapshotNamePrefix)
      } else {
        lines += line
      }
    }
    lines += ""
    addSnapshot()
    map
  }


}
