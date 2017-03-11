package tlang.repl

import java.io.{File, FileWriter}

import tlang.utils.Extensions._

import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scalaz.Cord

object CordExtension {


}

/**
  * Created by Tim Lindeberg on 3/11/2017.
  */
case class CommandHistory(maxRedoSize: Int, tabSize: Int) {


  private val HistorySeperator  = "★"
  private val HistoryFileName   = "repl_history"
  private val SettingsDirectory = System.getProperty("user.home") + File.separator + ".tlang"
  private val historyFile       = new File(SettingsDirectory, HistoryFileName)

  private val commands = CircularBuffer[Command](Command(maxRedoSize, tabSize))

  private val originalCommands = ArrayBuffer[String]()
  private var dirty            = false

  loadFromFile()

  def current: Command = commands.current

  def goToNext(): Unit = commands += 1
  def goToPrevious(): Unit = commands -= 1

  def saveCurrent(): Unit = {
    val command = current.currentCord
    if (command.isEmpty)
      return

    // Reset the used buffer
    commands.index match {
      case 0 => current.reset()
      case i => current.reset(originalCommands(i - 1))
    }
    commands.setPosition(0)

    // Remove previous if it already existed
    commands.indexWhere(c => equalCords(c.currentCord, command)) match {
      case -1    =>
      case index =>
        commands.remove(index)
        originalCommands.remove(index - 1)
    }

    dirty = true
    commands += Command(maxRedoSize, tabSize, command)
    originalCommands += command.toString
  }

  def saveToFile(): Unit = {
    if (!dirty)
      return

    dirty = false
    using(new FileWriter(historyFile)) { writer =>
      val seperator = "\n" + HistorySeperator + "\n"
      originalCommands.foreach { command =>
        writer.write(command)
        writer.write(seperator)
      }
    }
  }

  private def equalCords(c1: Cord, c2: Cord): Boolean = {
    if (c1.length != c2.length)
      return false
    val it1 = c1.self.iterator.flatMap(_.iterator)
    val it2 = c2.self.iterator.flatMap(_.iterator)
    while (it1.hasNext)
      if (it1.next != it2.next)
        return false
    true
  }

  private def loadFromFile(): Unit = {
    if (historyFile.getParentFile.mkdirs() || historyFile.createNewFile())
      return

    val lines = new ListBuffer[String]()
    using(io.Source.fromFile(historyFile)) {
      _.getLines().foreach { line =>
        if (line == HistorySeperator && lines.nonEmpty) {
          val str = lines.mkString("\n")
          originalCommands += str
          commands += Command(maxRedoSize, tabSize, Cord.empty :+ str)
          lines.clear()
        } else {
          lines += line
        }
      }
    }
  }

}

object CircularBuffer {
  def apply[T](ts: T*): CircularBuffer[T] =
    new CircularBuffer[T]().use { buffer => ts.foreach(buffer += _) }
}

class CircularBuffer[T] extends ArrayBuffer[T] {

  def index: Int = _index
  private var _index = 0

  def +=(i: Int): this.type = {_index = mod(_index + i, size); this}
  def -=(i: Int): this.type = {_index = mod(_index - i, size); this}

  def current: T = apply(_index)

  def setPosition(i: Int): Unit = _index = i

  private def mod(i: Int, m: Int): Int = {
    val x = i % m
    if (x < 0) x + m else x
  }

}
