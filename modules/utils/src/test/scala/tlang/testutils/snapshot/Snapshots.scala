package tlang
package testutils
package snapshot

import java.nio.charset.Charset

import better.files.File


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

  private var isDirty: Boolean = false

  // We use a LinkedHashMap to preserve the order of the snapshots in the tests
  private lazy val snapshots: mutable.LinkedHashMap[String, Snapshot] = readFromFile()

  def ++=(keyValues: Traversable[(String, String)]): Unit = keyValues foreach { this += _ }

  def +=(keyValue: (String, String)): Unit = keyValue match {
    case (name, newSnapshot) =>
      snapshots.get(name) match {
        case Some(snapshot) if snapshot.value == newSnapshot =>
        case _                                               => isDirty = true
      }
      snapshots(name) = Snapshot(newSnapshot, used = true)
  }

  def --=(keys: Traversable[String]): Unit = keys foreach { this -= _ }
  def -=(key: String): Unit = {
    if (!snapshots.contains(key))
      return

    isDirty = true
    snapshots -= key
  }

  def apply(name: String): Option[String] = {
    snapshots.get(name) map { case Snapshot(value, _) =>
      snapshots(name) = Snapshot(value, used = true)
      value
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
      .map { case (name, Snapshot(value, _)) => SnapshotNamePrefix + name + NL + value }
      .mkString(NL + NL) + NL

    file.parent.createDirectories()
    file.write(content)
  }

  override def toString: String = snapshots.toString().replace("Map", "Snapshots")

  private def readFromFile(): mutable.LinkedHashMap[String, Snapshot] = {
    val snapshots = mutable.LinkedHashMap[String, Snapshot]()

    if (!file.exists)
      return snapshots

    val lines = ListBuffer[String]()
    var name = ""

    def addSnapshot(): Unit = {
      if (name.nonEmpty)
        snapshots += name -> Snapshot(lines.dropRight(1).mkString(NL), used = false)
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
    snapshots
  }


}
