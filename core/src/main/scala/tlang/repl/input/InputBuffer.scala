package tlang.repl.input

import java.awt.datatransfer.{Clipboard, DataFlavor, StringSelection}

import tlang.repl.input.CordExtensions._
import tlang.utils.Extensions._
import tlang.utils.Position

import scalaz.Cord

object InputBuffer {

  def calculateLinePositions(cord: Cord): List[Int] = {
    val lines = cord.self.iterator.flatMap(_.iterator)
      .zipWithIndex
      .filter { case (c, _) => c == '\n' }
      .map { case (_, index) => index + 1 }
      .toList
    (0 :: lines).sortWith(_ >= _)
  }

  def apply(cord: Cord): InputBuffer = {
    val linePositions = calculateLinePositions(cord)
    apply(cord, Cursor(), Cursor(), linePositions)
  }

  def Empty = InputBuffer(Cord.empty)
}


case class InputBuffer(
  cord: Cord,
  mainCursor: Cursor,
  mark: Cursor,
  private val linePositions: List[Int],
  private val upDownX: Int = 0) {

  def selectedPosition: Position = {
    val cursors = List(mainCursor, mark).sorted
    Position(cursors(0).y + 1, cursors(0).x + 1, cursors(1).y + 1, cursors(1).x + 1)
  }

  def height: Int = linePositions.length

  def isEmpty: Boolean = cord.isEmpty

  def ++(str: String): InputBuffer = addChars(str)

  def +(char: Char): InputBuffer = char match {
    case '\n' => addChars("\n" + getIndentation)
    case _    => addChar(char)
  }

  def removeSelected(): InputBuffer = {
    if (isEmpty)
      return this

    val start = math.min(mainCursor.position, mark.position)
    val end = math.max(mainCursor.position, mark.position)
    var newLinePositions = linePositions

    val (newValue, newCursorPos) = if (start == end) {
      newLinePositions = newLinePositions.filter(_ != math.max(1, start))

      val v = if (start == cord.length) {
        cord.init
      } else if (start == 1) {
        cord.tail
      } else {
        val (before, after) = cord.split(start)
        before.init ++ after
      }
      (v, start - 1)
    } else {

      // Filter line positions in the removed area but don't remove the initial line
      val removedRange = math.max(1, start) until end
      newLinePositions = newLinePositions.filter(pos => !(pos - 1 in removedRange))

      val (before, _) = cord.split(start)
      val (_, after) = cord.split(end)
      (before ++ after, start)
    }

    val charsRemoved = cord.length - newValue.length

    // Realign line positions
    newLinePositions = newLinePositions.map { pos => if (pos > start) pos - charsRemoved else pos }

    val cursorPos = math.min(math.max(newCursorPos, 0), cord.length)

    val newCursor = getCursor(cursorPos, newLinePositions)
    InputBuffer(newValue, newCursor, newCursor, newLinePositions)
  }


  def paste(clipboard: Clipboard): InputBuffer = {
    this ++ clipboard.getData(DataFlavor.stringFlavor).asInstanceOf[String]
  }

  def copySelected(clipboard: Clipboard): InputBuffer = {
    val selected = if (mainCursor == mark) {
      val (start, end) = currentLinePosition
      System.lineSeparator + cord.slice(start, end).toString
    } else {
      val cursors = List(mainCursor, mark).sorted
      cord.slice(cursors(0).position, cursors(1).position).toString
    }

    val selection = new StringSelection(selected)
    clipboard.setContents(selection, selection)
    this
  }

  def cutSelected(clipboard: Clipboard): InputBuffer = {
    val buffer = if (mainCursor == mark) {
      val (start, end) = currentLinePosition
      copy(mainCursor = getCursor(math.max(0, start - 1), linePositions), mark = getCursor(end, linePositions))
    } else {
      this
    }
    buffer.copySelected(clipboard)
    buffer.removeSelected()
  }

  def removeToLeftWord(): InputBuffer = {
    val left = leftPosition
    var buffer = this
    while (buffer.mainCursor.position != left)
      buffer = buffer.removeSelected()
    buffer
  }

  def moveCursorHorizontal(steps: Int, moveMark: Boolean): InputBuffer = {
    moveCursor((mainCursor.position + steps).clamp(0, cord.size), moveMark)
  }

  def moveCursorVertical(steps: Int, moveMark: Boolean): InputBuffer = {
    val line = (currentLineIndex + steps).clamp(0, linePositions.length - 1)
    if (line == currentLineIndex)
      return this

    val newUpDownX = if (upDownX == 0) mainCursor.x else upDownX
    val (nextStart, nextEnd) = linePosition(line)
    moveCursor(nextStart + math.min(newUpDownX, nextEnd - nextStart), moveMark, newUpDownX)
  }

  def moveCursorToLeftWord(moveMark: Boolean): InputBuffer = moveCursor(leftPosition, moveMark)
  def moveCursorToRightWord(moveMark: Boolean): InputBuffer = moveCursor(rightPosition, moveMark)

  def currentLine: String = {
    val (start, end) = currentLinePosition
    if (start == end)
      return ""

    cord.slice(start, end).toString
  }

  override def toString: String = cord.toString
  def debugString: String =
    cord.toString.escape +
      "\n   Lines:   " + linePositions +
      "\n   Cursor:  " + mainCursor +
      "\n   Mark:    " + mark +
      "\n   UpDownX: " + upDownX

  private def addChars(chars: String): InputBuffer =
    chars.foldLeft(this) { case (current, char) => current.addChar(char) }

  private def addChar(c: Char): InputBuffer = {
    // Remove marked text
    val newBuffer = if (mainCursor != mark) removeSelected() else this
    newBuffer._addChar(c)
  }

  private def _addChar(char: Char): InputBuffer = {
    val position = mainCursor.position
    var newLinePositions = linePositions

    if (position != cord.length)
      newLinePositions = newLinePositions.map { pos => if (pos > position) pos + 1 else pos }

    if (char == '\n') {
      newLinePositions ::= position + 1
      newLinePositions = newLinePositions.sortWith(_ >= _)
    }

    val newMainCursor = getCursor(position + 1, newLinePositions)

    val newCord = if (cord.isEmpty || position == cord.length) {
      cord :- char
    } else if (position == 0) {
      cord.-:(char)
    } else {
      val (before, after) = cord.split(position)
      (before :- char) ++ after
    }
    InputBuffer(newCord, newMainCursor, newMainCursor, newLinePositions)
  }


  private def moveCursor(position: Int, moveMark: Boolean, upDownX: Int = 0): InputBuffer = {
    if (position == mainCursor.position && (!moveMark || mark == mainCursor))
      return this

    val newMainCursor = getCursor(position, linePositions)
    copy(
      mainCursor = newMainCursor,
      mark = if (moveMark) newMainCursor else mark,
      upDownX = upDownX
    )
  }

  private def leftPosition: Int = {
    val position = mainCursor.position

    if (isEmpty || position == 0)
      return position

    val leftOf = cord.split(position)._1
    val untilStop = charsUntilStop(leftOf.reverseIterator, position - 1)
    if (untilStop == -1) 0 else position - untilStop
  }

  private def rightPosition: Int = {
    val position = mainCursor.position

    if (isEmpty || position == cord.length)
      return position

    val rightOf = cord.split(position)._2
    val untilStop = charsUntilStop(rightOf.iterator, position)
    if (untilStop == -1) cord.length else position + untilStop
  }

  private def charsUntilStop(it: Iterator[Char], pos: Int): Int = {
    val currentChar = cord(math.max(0, pos))

    val currentIsWhiteSpace = currentChar.isWhitespace
    var i = 0
    while (it.hasNext) {
      val c = it.next()
      if (currentIsWhiteSpace != c.isWhitespace)
        return i
      i += 1
    }
    -1
  }


  private def getCursor(pos: Int, linePositions: List[Int] = linePositions): Cursor = {
    val index = lineIndex(pos, linePositions)
    val x = pos - linePositions(index)
    val y = linePositions.length - index - 1
    Cursor(pos, x, y)
  }

  private def currentLinePosition: (Int, Int) = linePosition(currentLineIndex)

  private def linePosition(line: Int): (Int, Int) = {
    val start = linePositions(line)
    val end = if (line == 0) cord.length else linePositions(line - 1) - 1
    (start, end)
  }

  private def currentLineIndex = lineIndex(mainCursor.position, linePositions)
  private def lineIndex(position: Int, linePositions: List[Int]) =
    linePositions.indices.find(i => linePositions(i) <= position).getOrElse(0)


  private def getIndentation = {
    val (start, end) = currentLinePosition
    val line = if (linePositions.size <= 1) cord else cord.slice(start, end)
    line.iterator.takeWhile(_ == '\t').mkString
  }
}
