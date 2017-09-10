package tlang.repl.input

import java.awt.datatransfer.{Clipboard, DataFlavor, StringSelection}

import tlang.repl.input.CordExtensions._
import tlang.utils.Extensions._
import tlang.utils.Position

import scalaz.Cord


case object InputState {

  val Empty = InputState(Cord.empty, List(0), 0)

}

case class InputState(cord: Cord, linePositions: List[Int], position: Int) {
  override def toString: String = {
    val text = '"' + cord.toString.escape + '"'
    val pos = position
    s"($text, $pos)"
  }
}


case class InputBuffer(
  cord: Cord = Cord.empty,
  mainCursor: Cursor = Cursor(),
  mark: Cursor = Cursor(),
  private val initialLinePositions: List[Int] = Nil,
  private val upDownX: Int = 0) {

  private val linePositions = if (initialLinePositions == Nil) calculateLinePositions(cord) else initialLinePositions

  println(debugString)

  def getMarkedPosition: Position = {
    val cursors = List(mainCursor, mark).sorted
    Position(cursors(0).y + 1, cursors(0).x + 1, cursors(1).y + 1, cursors(1).x + 1)
  }

  def height: Int = linePositions.length

  def isEmpty: Boolean = cord.isEmpty

  def ++(str: String): InputBuffer = addChars(str)

  def +(char: Char): InputBuffer = {
    val (start, end) = startAndEndOfCurrentLine
    val line = if (linePositions.size <= 1) cord else cord.slice(start, end)
    char match {
      case '\n' =>
        val indentation = line.iterator.takeWhile(_ == '\t').mkString
        addChars("\n" + indentation)
      case _    =>
        addChar(char)
    }
  }

  def removeOne(): InputBuffer = removeSelected()

  def paste(clipboard: Clipboard): InputBuffer = {
    this ++ clipboard.getData(DataFlavor.stringFlavor).asInstanceOf[String]
  }

  def copySelected(clipboard: Clipboard): InputBuffer = {
    val selected = if (mainCursor == mark) {
      val (start, end) = startAndEndOfCurrentLine
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
    if (mainCursor == mark) {
      val (start, end) = startAndEndOfCurrentLine
      getCursor(math.max(0, start - 1))
      getCursor(end)
    }
    copySelected(clipboard)
    removeSelected()
  }

  def removeToLeftWord(): InputBuffer = {
    val left = leftPosition
    var text = this
    while (text.mainCursor.position != left)
      text = removeSelected(text)
    text
  }


  def moveCursorLeft(steps: Int): InputBuffer = {
    val position = math.max(mainCursor.position - steps, 0)
    copy(mainCursor = getCursor(position), upDownX = 0)
  }

  def moveCursorRight(steps: Int): InputBuffer = {
    val position = math.min(mainCursor.position + steps, cord.size)
    copy(mainCursor = getCursor(position), upDownX = 0)
  }

  def moveCursorUp(): InputBuffer = {
    val currentLine = lineIndex
    if (currentLine == linePositions.length - 1)
      return this

    val (nextStart, nextEnd) = linePosition(currentLine + 1)
    copy(
      mainCursor = getCursor(nextStart + math.min(upDownX, nextEnd - nextStart)),
      upDownX = if (upDownX == 0) mainCursor.x else upDownX
    )
  }

  def moveCursorDown(): InputBuffer = {
    val currentLine = lineIndex
    if (currentLine == 0)
      return this

    val (nextStart, nextEnd) = linePosition(currentLine - 1)
    copy(
      mainCursor = getCursor(nextStart + math.min(upDownX, nextEnd - nextStart)),
      upDownX = if (upDownX == 0) mainCursor.x else upDownX
    )
  }


  def moveSecondaryCursor(): InputBuffer = {
    if (mark == mainCursor)
      return this

    copy(mark = mainCursor)
  }

  def goToLeftWord(): InputBuffer = copy(mainCursor = getCursor(leftPosition))
  def goToRightWord(): InputBuffer = copy(mainCursor = getCursor(rightPosition))

  def currentLine: String = {
    val (start, end) = startAndEndOfCurrentLine
    if (start == end)
      return ""

    cord.slice(start, end).toString
  }

  override def toString: String = cord.toString
  def debugString: String =
    "'" + cord.toString.escape +
      "'\n   Lines:  " + linePositions +
      "\n   Cursor: " + mainCursor +
      "\n   Mark:   " + mark

  private def addChars(chars: String): InputBuffer =
    chars.foldLeft(this) { case (current, char) => addChar(char, current) }

  private def addChar(char: Char, buffer: InputBuffer = this): InputBuffer = {

    // Remove marked text
    val newBuffer = if (mainCursor != mark) removeSelected(buffer) else buffer

    val text = newBuffer.cord
    val position = newBuffer.mainCursor.position
    var newLinePositions = newBuffer.linePositions

    if (position != text.length)
      newLinePositions = newLinePositions.map { pos => if (pos > position) pos + 1 else pos }

    if (char == '\n') {
      newLinePositions = (position + 1 :: newLinePositions).sortWith(_ >= _)
    }

    val newMainCursor = getCursor(position + 1)

    val newCord = if (text.isEmpty || position == text.length) {
      text :- char
    } else if (position == 0) {
      text.-:(char)
    } else {
      val (before, after) = text.split(position)
      (before :- char) ++ after
    }
    InputBuffer(newCord, newMainCursor, newMainCursor, newLinePositions)
  }

  // Removes the content between the start cursor and the end cursor
  private def removeSelected(buffer: InputBuffer = this): InputBuffer = {
    if (buffer.isEmpty)
      return buffer

    val InputBuffer(text, mainCursor, mark, _, _) = buffer

    val start = math.min(mainCursor.position, mark.position)
    val end = math.max(mainCursor.position, mark.position)
    var newLinePositions = buffer.linePositions

    val (newValue, newCursorPos) = if (start == end) {
      newLinePositions = newLinePositions.filter(_ != math.max(1, start))

      val v = if (start == text.length) {
        text.init
      } else if (start == 1) {
        text.tail
      } else {
        val (before, after) = text.split(start)
        before.init ++ after
      }
      (v, start - 1)
    } else {

      // Filter linepositions in the removed area but don't remove the initial line
      val removedRange = math.max(1, start) until end
      newLinePositions = newLinePositions.filter(pos => !(pos - 1 in removedRange))

      val (before, _) = text.split(start)
      val (_, after) = text.split(end)
      (before ++ after, start)
    }

    val charsRemoved = text.length - newValue.length

    // Realign line positions
    newLinePositions = newLinePositions.map { pos => if (pos > start) pos - charsRemoved else pos }

    val cursorPos = math.min(math.max(newCursorPos, 0), cord.length)

    val newCursor = getCursor(cursorPos)
    InputBuffer(newValue, newCursor, newCursor, newLinePositions)
  }

  private def leftPosition: Int = {
    val text = cord
    val position = mainCursor.position

    if (text.isEmpty || position == 0)
      return position

    val leftOf = text.split(position)._1
    val untilStop = charsUntilStop(leftOf.reverseIterator, position - 1)
    if (untilStop == -1) 0 else position - untilStop
  }

  private def rightPosition: Int = {
    val text = cord
    val position = mainCursor.position

    if (text.isEmpty || position == text.length)
      return position

    val rightOf = text.split(position)._2
    val untilStop = charsUntilStop(rightOf.iterator, position)
    if (untilStop == -1) text.length else position + untilStop
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

  private def calculateLinePositions(cord: Cord): List[Int] = {
    0 :: cord.self.iterator.flatMap(_.iterator)
      .zipWithIndex
      .filter { case (c, _) => c == '\n' }
      .map { case (_, index) => index + 1 }
      .toList
      .sortWith(_ >= _)
  }

  private def getCursor(pos: Int): Cursor = {
    val index = lineIndex
    val x = pos - linePositions(index)
    val y = linePositions.length - index - 1
    Cursor(pos, x, y)
  }

  private def startAndEndOfCurrentLine: (Int, Int) = {
    val line = lineIndex
    val start = linePositions(line)

    if (line == 0)
      return (start, cord.length)

    (start, linePositions(line - 1) - 1)
  }

  private def linePosition(line: Int): (Int, Int) = {
    val start = linePositions(line)
    val end = if (line == 0) cord.length else linePositions(line - 1) - 1
    (start, end)
  }

  private def lineIndex = linePositions.indices.find(i => linePositions(i) <= mainCursor.position).getOrElse(0)


}
