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

  private val commands = CircularBuffer(newHistory())

  private val unmodifiedCommands = ArrayBuffer[String]()
  private var dirty              = false

  loadFromFile()

  def size: Int = commands.size

  def currentHistory: History[InputBuffer] = commands.current
  def currentCommand: InputBuffer = currentHistory.current.get

  def changeCommand(steps: Int): this.type = { commands advance steps; this }

  def undo(): Boolean = currentHistory.undo()
  def redo(): Boolean = currentHistory.redo()

  // Forward operations to the underlying buffer
  def +=(char: Char): this.type = setCurrent(currentCommand + char, saveHistory = true)
  def ++=(chars: String): this.type = setCurrent(currentCommand ++ chars, saveHistory = true)
  def removeSelected(): this.type = setCurrent(currentCommand.removeSelected(), saveHistory = true)

  def paste(): this.type = setCurrent(currentCommand.paste(clipboard), saveHistory = true)
  def copySelected(): this.type = setCurrent(currentCommand.copySelected(clipboard), saveHistory = false)
  def cutSelected(): this.type = setCurrent(currentCommand.cutSelected(clipboard), saveHistory = true)
  def removeToLeftWord(): this.type = setCurrent(currentCommand.removeToLeftWord(), saveHistory = true)

  def left(shiftDown: Boolean): this.type = setCurrent(currentCommand.moveCursorHorizontal(-1, !shiftDown), saveHistory = false)
  def right(shiftDown: Boolean): this.type = setCurrent(currentCommand.moveCursorHorizontal(1, !shiftDown), saveHistory = false)
  def goToLeftWord(shiftDown: Boolean): this.type = setCurrent(currentCommand.moveCursorToLeftWord(!shiftDown), saveHistory = false)
  def goToRightWord(shiftDown: Boolean): this.type = setCurrent(currentCommand.moveCursorToRightWord(!shiftDown), saveHistory = false)
  def up(shiftDown: Boolean): this.type = upOrDown(1, shiftDown)
  def down(shiftDown: Boolean): this.type = upOrDown(-1, shiftDown)

  def saveCurrentCommand(): Unit = {
    val command = currentCommand
    if (command.isEmpty)
      return

    dirty = true

    // Reset the used buffer back to it's original state
    commands.index match {
      case 0 => setCurrent(InputBuffer.Empty, saveHistory = false)
      case i => setCurrent(InputBuffer(unmodifiedCommands(i - 1)), saveHistory = false)
    }
    commands.setPosition(0)

    // Remove earlier duplicate command if it exists
    commands.indexWhere(c => c.current.get.cord equalCord command.cord) match {
      case -1    =>
      case index =>
        commands.remove(index)
        unmodifiedCommands.remove(index - 1)
    }

    commands += newHistory(command)
    unmodifiedCommands += command.toString
  }

  def saveToFile(): Unit = {
    if (!dirty)
      return

    dirty = false
    val content = unmodifiedCommands.mkString(Seperator) + Seperator
    historyFile.write(content)
  }

  private def upOrDown(direction: Int, shiftDown: Boolean): this.type = {
    val newBuffer = currentCommand.moveCursorVertical(direction, !shiftDown)
    if (newBuffer eq currentCommand)
      commands advance -direction
    else
      setCurrent(newBuffer, saveHistory = false)
    this
  }

  private def setCurrent(buffer: InputBuffer, saveHistory: Boolean): this.type = {
    if (saveHistory)
      currentHistory += buffer
    else
      currentHistory.replaceCurrent(buffer)
    this
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
        commands += newHistory(InputBuffer(Cord.empty :+ str))
        lines.clear()
      } else {
        lines += line
      }
    }
  }

  private def newHistory(buffer: InputBuffer = InputBuffer.Empty) = History(maxHistorySize, buffer)

}
