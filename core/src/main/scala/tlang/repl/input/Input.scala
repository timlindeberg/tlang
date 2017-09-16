package tlang.repl.input


import better.files.File
import tlang.repl.input.CordExtensions._
import tlang.utils.CircularBuffer
import tlang.utils.Extensions._

import scala.collection.mutable.{ArrayBuffer, ListBuffer}

object Input {

  val HistorySeperator = "<--COMMAND-->"
  val Seperator        = NL + HistorySeperator + NL

}

case class Input(historyFile: File, clipboard: Clipboard, maxHistorySize: Int) {

  import Input._

  private val commands = CircularBuffer(newHistory())

  private val unmodifiedCommands = ArrayBuffer[String]()
  private var dirty              = false

  loadFromFile()

  def size: Int = commands.size

  def currentBuffer: InputBuffer = currentHistory.current.get

  def undo(): Boolean = currentHistory.undo()
  def redo(): Boolean = currentHistory.redo()

  override def toString: String = currentBuffer.toString

  // Forward operations to the underlying buffer
  def +=(char: Char): this.type = add(char)
  def add(char: Char): this.type = {
    val newBuffer = char match {
      case '\n' =>
        val indentation = currentBuffer.currentLine.takeWhile(_ == '\t').mkString
        currentBuffer ++ ('\n' + indentation)
      case _    => currentBuffer + char
    }
    setCurrent(newBuffer, saveHistory = true)
  }

  def ++=(chars: String): this.type = add(chars)
  def add(chars: String): this.type = setCurrent(currentBuffer ++ chars, saveHistory = true)

  def removeSelected(): this.type = setCurrent(currentBuffer.removeSelected(), saveHistory = true)

  def paste(): this.type = setCurrent(currentBuffer ++ clipboard.content, saveHistory = true)
  def copySelected(): this.type = {
    currentBuffer.selected match {
      case ""        =>
        val buffer = currentBuffer.selectCurrentLine
        clipboard.setContent(buffer.selected)
        setCurrent(buffer, saveHistory = false)
      case selection =>
        clipboard.setContent(selection)
    }
    this
  }
  def cutSelected(): this.type = {
    copySelected()
    setCurrent(currentBuffer.removeSelected(), saveHistory = true)
  }

  def removeToLeftWord(): this.type = {
    val moved = currentBuffer.moveCursorToLeftWord(moveSecondary = false)
    setCurrent(moved.removeSelected(), saveHistory = true)
  }

  def left(shiftDown: Boolean = false): this.type = setCurrent(currentBuffer.moveCursorHorizontal(-1, !shiftDown), saveHistory = false)
  def right(shiftDown: Boolean = false): this.type = setCurrent(currentBuffer.moveCursorHorizontal(1, !shiftDown), saveHistory = false)
  def goToLeftWord(shiftDown: Boolean = false): this.type = setCurrent(currentBuffer.moveCursorToLeftWord(!shiftDown), saveHistory = false)
  def goToRightWord(shiftDown: Boolean = false): this.type = setCurrent(currentBuffer.moveCursorToRightWord(!shiftDown), saveHistory = false)
  def up(shiftDown: Boolean = false): this.type = upOrDown(1, shiftDown)
  def down(shiftDown: Boolean = false): this.type = upOrDown(-1, shiftDown)

  def saveCurrentCommand(): this.type = {
    val command = currentBuffer
    if (command.isEmpty)
      return this

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
    this
  }

  def saveToFile(): this.type = {
    if (!dirty)
      return this

    dirty = false
    val content = unmodifiedCommands.mkString(Seperator) + Seperator
    historyFile.write(content)
    this
  }

  private def currentHistory: History[InputBuffer] = commands.current

  private def upOrDown(direction: Int, shiftDown: Boolean): this.type = {
    val newBuffer = currentBuffer.moveCursorVertical(direction, !shiftDown)
    if (newBuffer eq currentBuffer)
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
        val str = lines.mkString(NL)
        unmodifiedCommands += str
        commands += newHistory(InputBuffer(str))
        lines.clear()
      } else {
        lines += line
      }
    }
  }

  private def newHistory(buffer: InputBuffer = InputBuffer.Empty) = History(maxHistorySize, buffer)

}
