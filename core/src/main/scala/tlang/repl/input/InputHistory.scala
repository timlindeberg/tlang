package tlang.repl.input


import better.files._

import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scalaz.Cord

object InputHistory {

  val HistorySeperator = "<--INPUT_HISTORY-->"
  val Seperator        = System.lineSeparator + HistorySeperator + System.lineSeparator

}

case class InputHistory(historyFile: File, maxHistorySize: Int, tabSize: Int) {

  import InputHistory._

  private val commands = CircularBuffer[InputBuffer](InputBuffer(maxHistorySize, tabSize))

  private val unmodifiedCommands = ArrayBuffer[String]()
  private var dirty              = false

  loadFromFile()

  def size: Int = commands.size
  def current: InputBuffer = commands.current

  def goToNext(): Unit = commands advance 1
  def goToPrevious(): Unit = commands advance -1

  def saveCurrent(): Unit = {
    val command = current.currentCord
    if (command.isEmpty)
      return

    dirty = true

    // Reset the used buffer back to it's original state
    commands.index match {
      case 0 => current.reset()
      case i => current.reset(unmodifiedCommands(i - 1))
    }
    commands.setPosition(0)

    // Remove earlier duplicate command if it exists
    commands.indexWhere(c => equalCords(c.currentCord, command)) match {
      case -1    =>
      case index =>
        commands.remove(index)
        unmodifiedCommands.remove(index - 1)
    }

    commands += InputBuffer(maxHistorySize, tabSize, command)
    unmodifiedCommands += command.toString
  }

  def saveToFile(): Unit = {
    if (!dirty)
      return

    dirty = false
    val content = unmodifiedCommands.mkString(Seperator) + Seperator
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
    historyFile.lineIterator.foreach { line =>
      if (line == HistorySeperator && lines.nonEmpty) {
        val str = lines.mkString(System.lineSeparator)
        unmodifiedCommands += str
        commands += InputBuffer(maxHistorySize, tabSize, Cord.empty :+ str)
        lines.clear()
      } else {
        lines += line
      }
    }
  }

}
