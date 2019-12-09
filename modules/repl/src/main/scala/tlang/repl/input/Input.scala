package tlang
package repl
package input

import better.files.File
import tlang.repl.input.CordExtensions._
import tlang.utils.CircularBuffer

import scala.collection.mutable.{ArrayBuffer, ListBuffer}

object Input {

  val HistorySeperator = "<--COMMAND-->"
  val Seperator: String = NL + HistorySeperator + NL
}

case class Input(historyFile: File, clipboard: Clipboard, maxHistorySize: Int, tabWidth: Int) {

  import Input._

  private val commands = CircularBuffer(newHistory())

  private val unmodifiedCommands = ArrayBuffer[String]()
  private var dirty = false

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
        val buffer = currentBuffer.selectCurrentLine(selectNewLine = true)
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

  def left(altDown: Boolean = false, shiftDown: Boolean = false): this.type = leftOrRight(-1, altDown, shiftDown)
  def right(altDown: Boolean = false, shiftDown: Boolean = false): this.type = leftOrRight(1, altDown, shiftDown)
  def up(shiftDown: Boolean = false): this.type = upOrDown(1, shiftDown)
  def down(shiftDown: Boolean = false): this.type = upOrDown(-1, shiftDown)

  def moveCursorToStartOfLine(shiftDown: Boolean = false): this.type = startOrEndOfLine(-1, shiftDown)
  def moveCursorToEndOfLine(shiftDown: Boolean = false): this.type = startOrEndOfLine(1, shiftDown)

  def moveCursorTo(x: Int, y: Int, moveSecondary: Boolean): this.type = {
    setCurrent(currentBuffer.moveCursor(x, y, moveSecondary), saveHistory = false)
  }

  def removeToStartOfLine(): this.type = {
    val (start, _) = currentBuffer.currentLinePosition
    val movedBuffer = currentBuffer.moveCursor(start, moveSecondary = false)
    setCurrent(movedBuffer.removeSelected(), saveHistory = true)
  }

  def selectLine(x: Int, y: Int): this.type = {
    val moved = currentBuffer.moveCursor(x, y)
    setCurrent(moved.selectCurrentLine(), saveHistory = false)
  }

  def selectWord(x: Int, y: Int): this.type = {
    val moved = currentBuffer.moveCursor(x, y)
    setCurrent(moved.selectCurrentWord(), saveHistory = false)
  }

  def saveCurrentCommand(): this.type = {
    val command = currentBuffer
    if (command.isEmpty)
      return this

    dirty = true

    // Reset the used buffer back to it's original state
    commands.index match {
      case 0 => setCurrent(InputBuffer(tabWidth), saveHistory = false)
      case i => setCurrent(InputBuffer(unmodifiedCommands(i - 1), tabWidth), saveHistory = false)
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

  private def leftOrRight(direction: Int, altDown: Boolean, shiftDown: Boolean): this.type = {
    val moveSecondary = !shiftDown
    val newBuffer = if (altDown)
      if (direction == 1)
        currentBuffer.moveCursorToRightWord(moveSecondary)
      else
        currentBuffer.moveCursorToLeftWord(moveSecondary)
    else
      currentBuffer.moveCursorHorizontal(direction, moveSecondary)
    setCurrent(newBuffer, saveHistory = false)
  }

  private def startOrEndOfLine(direction: Int, shiftDown: Boolean): this.type = {
    val (start, end) = currentBuffer.currentLinePosition
    val pos = if (direction == 1) end else start
    setCurrent(currentBuffer.moveCursor(pos, moveSecondary = !shiftDown), saveHistory = false)
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
        commands += newHistory(InputBuffer(str, tabWidth))
        lines.clear()
      } else {
        lines += line
      }
    }
  }

  private def newHistory(buffer: InputBuffer = InputBuffer(tabWidth)) = History(maxHistorySize, buffer)
}
