package tlang.repl.input


import java.awt.datatransfer.Clipboard

import better.files.File
import tlang.repl.input.CordExtensions._
import tlang.utils.CircularBuffer

import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scalaz.Cord

object Input {

  val HistorySeperator = "<--INPUT_HISTORY-->"
  val Seperator        = System.lineSeparator + HistorySeperator + System.lineSeparator

}

case class Input(historyFile: File, clipboard: Clipboard, maxHistorySize: Int, tabSize: Int) {

  import Input._

  private val commands = CircularBuffer[InputBuffer](InputBuffer())

  private val unmodifiedCommands = ArrayBuffer[String]()
  private var dirty              = false

  loadFromFile()

  def size: Int = commands.size

  def current: InputBuffer = commands.current

  def changeToNextCommand(): Unit = commands advance 1
  def changeToPreviousCommand(): Unit = commands advance -1

  def undo(): Boolean = true
  def redo(): Boolean = true

  // Forward operations to the underlying buffer
  def +=(char: Char): this.type = setCurrent(current + char)
  def ++=(chars: String): this.type = setCurrent(current ++ chars)
  def backspace(): this.type = setCurrent(current.removeOne())

  def paste(): this.type = setCurrent(current.paste(clipboard))
  def copySelected(): this.type = { current.copySelected(clipboard); this }
  def cutSelected(): this.type = setCurrent(current.cutSelected(clipboard))
  def removeToLeftWord(): this.type = setCurrent(current.removeToLeftWord())
  def moveSecondaryCursor(): this.type = setCurrent(current.moveSecondaryCursor())

  def left(): this.type = setCurrent(current.moveCursorLeft(1))
  def right(): this.type = setCurrent(current.moveCursorRight(1))

  def up(): Boolean = upOrDown(current.moveCursorUp())
  def down(): Boolean = upOrDown(current.moveCursorDown())

  def goToLeftWord(): this.type = setCurrent(current.goToLeftWord())
  def goToRightWord(): this.type = setCurrent(current.goToRightWord())

  def saveCurrent(): Unit = {
    val command = current
    if (command.isEmpty)
      return

    dirty = true

    // Reset the used buffer back to it's original state
    commands.index match {
      case 0 => setCurrent(InputBuffer())
      case i => setCurrent(InputBuffer(cord = unmodifiedCommands(i - 1)))
    }
    commands.setPosition(0)

    // Remove earlier duplicate command if it exists
    commands.indexWhere(c => c.cord equalCord command.cord) match {
      case -1    =>
      case index =>
        commands.remove(index)
        unmodifiedCommands.remove(index - 1)
    }

    commands += command
    unmodifiedCommands += command.toString
  }

  def saveToFile(): Unit = {
    if (!dirty)
      return

    dirty = false
    val content = unmodifiedCommands.mkString(Seperator) + Seperator
    historyFile.write(content)
  }

  private def upOrDown(newBuffer: => InputBuffer): Boolean = {
    if (newBuffer eq current)
      return false
    setCurrent(newBuffer)
    true
  }

  private def setCurrent(buffer: InputBuffer): this.type = { commands.setCurrent(buffer); this }

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
        commands += InputBuffer(Cord.empty :+ str)
        lines.clear()
      } else {
        lines += line
      }
    }
  }

}
