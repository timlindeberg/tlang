package tlang.repl.input


import better.files._

import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scalaz.Cord

case class InputHistory(historyFile: File, maxRedoSize: Int, tabSize: Int) {

  private val HistorySeperator = "★"
  private val Seperator        = System.lineSeparator + HistorySeperator + System.lineSeparator


  private val commands = CircularBuffer[InputBuffer](InputBuffer(maxRedoSize, tabSize))

  private val originalCommands = ArrayBuffer[String]()
  private var dirty            = false

  loadFromFile()

  def current: InputBuffer = commands.current

  def goToNext(): Unit = commands advance 1
  def goToPrevious(): Unit = commands advance -1

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
    val content = originalCommands.mkString(Seperator) + Seperator
    historyFile.write(content)
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
    if (!historyFile.exists) {
      historyFile.createIfNotExists(createParents = true)
      return
    }

    val lines = new ListBuffer[String]()
    historyFile.lines.foreach { line =>
      if (line == HistorySeperator && lines.nonEmpty) {
        val str = lines.mkString(System.lineSeparator)
        originalCommands += str
        commands += InputBuffer(maxRedoSize, tabSize, Cord.empty :+ str)
        lines.clear()
      } else {
        lines += line
      }
    }
  }

}
