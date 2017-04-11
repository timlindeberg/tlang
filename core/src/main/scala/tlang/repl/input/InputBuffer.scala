package tlang.repl.input

import java.awt.Toolkit
import java.awt.datatransfer.{Clipboard, DataFlavor, StringSelection}

import tlang.repl.input.CordExtensions._
import tlang.utils.Extensions._
import tlang.utils.Position

import scalaz.Cord


case class InputState(cord: Cord, linePositions: List[Int], position: Int)

case class InputBuffer(maxHistorySize: Int, tabSize: Int, private val cord: Cord = Cord.empty) {

  private val systemClipboard: Clipboard = Toolkit.getDefaultToolkit.getSystemClipboard


  private val history = UndoHistory(maxHistorySize)

  private var linePositions: List[Int] = _
  private var upDownX      : Int       = _

  val mainCursor = Cursor()
  val mark       = Cursor()

  def getMarkedPosition: Position = {
    val cursors = List(mainCursor, mark).sorted
    Position(cursors(0).y + 1, cursors(0).x + 1, cursors(1).y + 1, cursors(1).x + 1)
  }

  def height: Int = linePositions.length

  def currentCord: Cord = history.current.cord

  reset(cord)

  def lines: Int = linePositions.size

  def ++=(str: String): Unit = {
    upDownX = 0

    val newCord = addChars(str)
    history += InputState(newCord, linePositions, mainCursor.position)
  }

  def +=(char: Char): Unit = {
    upDownX = 0

    val (start, end) = startAndEndOfLine
    val line = if (linePositions.size <= 1) currentCord else currentCord.slice(start, end)
    val newCord = char match {
      case '\t'                                        =>
        val numSpaces = tabSize - (mainCursor.x % tabSize)
        addChars(" " * numSpaces)
      case '\n'                                        =>
        var indent = " " * line.iterator.indexWhere(!_.isWhitespace)
        if (line.nonEmpty && line(line.length - 1) == '{')
          indent += " " * tabSize

        addChars("\n" + indent)
      case '}' if line.iterator.forall(_.isWhitespace) =>
        val trimmed = (0 until tabSize).foldLeft(currentCord) { case (current, _) => _remove(current) }
        addChar('}', trimmed)
      case _                                           =>
        addChar(char)
    }
    history += InputState(newCord, linePositions, mainCursor.position)

  }

  def remove(): Unit = {
    val newValue = _remove()
    history += InputState(newValue, linePositions, mainCursor.position)
  }

  def paste(): Unit = {
    this ++= systemClipboard.getData(DataFlavor.stringFlavor).asInstanceOf[String]
  }

  def copy(): Unit = {
    val selected = if (mainCursor == mark) {
      val (start, end) = startAndEndOfLine
      "\n" + currentCord.slice(start, end).toString
    } else {
      val cursors = List(mainCursor, mark).sorted
      currentCord.slice(cursors(0).position, cursors(1).position).toString
    }

    val selection = new StringSelection(selected)
    systemClipboard.setContents(selection, selection)
  }

  def cut(): Unit = {
    if (mainCursor == mark) {
      val (start, end) = startAndEndOfLine
      setPos(mainCursor, math.max(0, start - 1))
      setPos(mark, end)
    }
    copy()
    val text = _remove()
    history += InputState(text, linePositions, mainCursor.position)
  }

  def removeToLeftWord(): Unit = {
    upDownX = 0
    val left = leftPosition
    var text = currentCord
    while (mainCursor.position != left)
      text = _remove(text)
    history += InputState(text, linePositions, mainCursor.position)
  }

  def setMark(): Unit = {
    mark.position = mainCursor.position
    mark.x = mainCursor.x
    mark.y = mainCursor.y
  }

  def goToLeftWord(): Unit = setPos(mainCursor, leftPosition)
  def goToRightWord(): Unit = setPos(mainCursor, rightPosition)

  def moveLeft(count: Int): Unit = {
    upDownX = 0

    val position = math.max(mainCursor.position - count, 0)
    setPos(mainCursor, position)
  }

  def moveRight(count: Int): Unit = {
    upDownX = 0

    val position = math.min(mainCursor.position + count, history.current.cord.size)
    setPos(mainCursor, position)
  }

  def up(): Boolean = {
    val currentLine = lineIndex
    if (currentLine == linePositions.length - 1)
      return false

    if (upDownX == 0)
      upDownX = mainCursor.x


    val (nextStart, nextEnd) = linePosition(currentLine + 1)
    setPos(mainCursor, nextStart + math.min(upDownX, nextEnd - nextStart))
    true
  }

  def down(): Boolean = {
    val currentLine = lineIndex
    if (currentLine == 0)
      return false

    if (upDownX == 0)
      upDownX = mainCursor.x

    val (nextStart, nextEnd) = linePosition(currentLine - 1)
    setPos(mainCursor, nextStart + math.min(upDownX, nextEnd - nextStart))
    true
  }

  def undo(): Boolean = updateState(history.undo)
  def redo(): Boolean = updateState(history.redo)

  def reset(cord: Cord = Cord.empty): Unit = {
    mainCursor.reset()
    upDownX = 0
    linePositions = List(0)
    linePositions ++= getLinePositions(cord)
    linePositions = linePositions.sortWith(_ >= _)
    history.clear()
    history += InputState(cord, linePositions, 0)
  }

  def currentLine: String = {
    val (start, end) = startAndEndOfLine
    if (start == end)
      return ""

    currentCord.slice(start, end).toString
  }

  override def toString: String = currentCord.toString
  def debugString: String = "Lines: " + linePositions + "\n" + history

  private def addChars(chars: String, text: Cord = currentCord): Cord =
    chars.foldLeft(text) { case (current, char) => addChar(char, current) }

  private def addChar(char: Char, cord: Cord = currentCord): Cord = {

    // Remove marked text
    val text = if (mainCursor != mark) _remove() else cord

    val position = mainCursor.position
    if (position != text.length)
      linePositions = linePositions.map { pos => if (pos > position) pos + 1 else pos }

    if (char == '\n') {
      linePositions ::= position + 1
      linePositions = linePositions.sortWith(_ >= _)
    }

    setPos(mainCursor, position + 1)
    setPos(mark, position + 1)

    if (text.isEmpty || position == text.length) {
      text :- char
    } else if (position == 0) {
      text.-:(char)
    } else {
      val (before, after) = text.split(position)
      (before :- char) ++ after
    }
  }

  private def _remove(text: Cord = currentCord): Cord = {
    if (text.isEmpty)
      return text


    val start = math.min(mainCursor.position, mark.position)
    val end = math.max(mainCursor.position, mark.position)

    val (newValue, newCursorPos) = if (start == end) {
      linePositions = linePositions.filter(_ != math.max(1, start))

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

      linePositions = linePositions.filter(pos => !(pos - 1 in removedRange))

      val (before, _) = text.split(start)
      val (_, after) = text.split(end)
      (before ++ after, start)
    }

    val charsRemoved = text.length - newValue.length

    // Realign line positions
    linePositions = linePositions.map { pos => if (pos > start) pos - charsRemoved else pos }

    val cursorPos = math.min(math.max(newCursorPos, 0), currentCord.length)

    setPos(mainCursor, cursorPos)
    setPos(mark, cursorPos)
    newValue
  }

  private def leftPosition: Int = {
    val text = currentCord
    val position = mainCursor.position

    if (text.isEmpty || position == 0)
      return position

    val leftOf = text.split(position)._1
    val untilStop = charsUntilStop(leftOf.reverseIterator, position - 1)
    if (untilStop == -1) 0 else position - untilStop
  }

  private def rightPosition: Int = {
    val text = currentCord
    val position = mainCursor.position

    if (text.isEmpty || position == text.length)
      return position

    val rightOf = text.split(position)._2
    val untilStop = charsUntilStop(rightOf.iterator, position)
    if (untilStop == -1) text.length else position + untilStop
  }

  private def charsUntilStop(it: Iterator[Char], pos: Int): Int = {
    val currentChar = currentCord(math.max(0, pos))

    val currentIsWhiteSpace = currentChar.isWhitespace
    var i = 0
    while (it.hasNext) {
      val c = it.next()
      if (currentIsWhiteSpace != c.isWhitespace) return i
      i += 1
    }
    -1
  }

  private def getLinePositions(cord: Cord): List[Int] = {
    cord.self.iterator.flatMap(_.iterator)
      .zipWithIndex
      .filter { case (c, _) => c == '\n' }
      .map { case (_, index) => index + 1 }
      .toList
  }

  private def setPos(cursor: Cursor, pos: Int): Unit = {
    cursor.position = pos
    val index = lineIndex

    cursor.x = pos - linePositions(index)
    cursor.y = linePositions.length - index - 1
  }

  private def startAndEndOfLine: (Int, Int) = {
    val line = lineIndex
    val start = linePositions(line)

    if (line == 0)
      return (start, currentCord.length)

    (start, linePositions(line - 1) - 1)
  }

  private def linePosition(line: Int): (Int, Int) = {
    val start = linePositions(line)
    val end = if (line == 0) history.current.cord.length else linePositions(line - 1) - 1
    (start, end)
  }

  private def lineIndex = linePositions.indices.find(i => linePositions(i) <= mainCursor.position).getOrElse(0)

  private def updateState(f: () => Boolean): Boolean = {
    if (!f())
      return false

    upDownX = 0
    val newState = history.current
    setPos(mainCursor, newState.position)
    linePositions = newState.linePositions
    true
  }


}
