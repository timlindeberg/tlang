package tlang.testutils.snapshot

import java.nio.charset.Charset

import better.files.File
import tlang.utils.Extensions._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object Snapshots {
  val SnapshotNamePrefix: String = "========================================= "
}

case class Snapshot(value: String, used: Boolean)

case class Snapshots(file: File) {

  import Snapshots._

  // Without this here the file is not parsed correctly. Seems like a bug in betterfiles
  private implicit val encoding: Charset = Charset.defaultCharset()

  private      var isDirty  : Boolean                       = false
  private lazy val snapshots: mutable.Map[String, Snapshot] = parse

  def +=(keyValue: (String, String)): Unit = keyValue match {
    case (name, newSnapshot) =>
      snapshots.get(name) match {
        case Some(snapshot) if snapshot.value == newSnapshot =>
        case _                                               => isDirty = true
      }
      snapshots(name) = Snapshot(newSnapshot, used = true)
  }

  def -=(key: String): Unit = {
    if (!snapshots.contains(key))
      return

    isDirty = true
    snapshots -= key
  }

  def apply(name: String): Option[String] = {
    snapshots.get(name) match {
      case Some(Snapshot(value, _)) =>
        snapshots(name) = Snapshot(value, used = true)
        Some(value)
      case _                        => None
    }
  }

  def unused(matchingName: Option[String]): List[String] = {
    snapshots.toList
      .filter { case (name, Snapshot(_, used)) => !used && (matchingName.isEmpty || name.startsWith(matchingName.get)) }
      .map { case (name, _) => name }
  }

  def save(): Unit = {
    if (!isDirty)
      return

    val content = snapshots
      .toSeq
      .sortBy { case (name, _) => name }
      .map { case (name, Snapshot(value, _)) => SnapshotNamePrefix + name + NL + value }
      .mkString(NL + NL) + NL

    file.parent.createDirectories()
    file.write(content)
  }

  override def toString: String = snapshots.toString().replace("Map", "Snapshots")


  private def parse: mutable.Map[String, Snapshot] = {
    val map = mutable.Map[String, Snapshot]()

    if (!file.exists)
      return map

    val lines = ListBuffer[String]()
    var name = ""

    def addSnapshot(): Unit = {
      if (name.nonEmpty)
        map += name -> Snapshot(lines.dropRight(1).mkString(NL), used = false)
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
