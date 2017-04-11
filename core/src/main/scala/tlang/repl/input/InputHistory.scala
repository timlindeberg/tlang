package tlang.repl.input

import java.io.{File, FileWriter}

import tlang.utils.Extensions._

import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scalaz.Cord

case class InputHistory(maxRedoSize: Int, tabSize: Int) {

  private val HistorySeperator  = "★"
  private val HistoryFileName   = "repl_history"
  private val SettingsDirectory = System.getProperty("user.home") + File.separator + ".tlang"
  private val historyFile       = new File(SettingsDirectory, HistoryFileName)

  private val commands = CircularBuffer[InputBuffer](InputBuffer(maxRedoSize, tabSize))

  private val originalCommands = ArrayBuffer[String]()
  private var dirty            = false

  loadFromFile()

  def current: InputBuffer = commands.current

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
    commands += InputBuffer(maxRedoSize, tabSize, command)
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
          commands += InputBuffer(maxRedoSize, tabSize, Cord.empty :+ str)
          lines.clear()
        } else {
          lines += line
        }
      }
    }
  }

}
