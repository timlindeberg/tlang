package tlang.repl.input

import java.awt.Toolkit
import java.awt.datatransfer.{Clipboard, DataFlavor, StringSelection}

import tlang.repl.input.CordExtensions._
import tlang.utils.Extensions._
import tlang.utils.Position

import scalaz.Cord

object Cursor {
  def apply(x: Int, y: Int): Cursor = Cursor(0, x, y)
  def apply(): Cursor = Cursor(0, 0, 0)

}
case class Cursor(var position: Int, var x: Int, var y: Int) extends Ordered[Cursor] {

  def reset(): Unit = {
    position = 0
    x = 0
    y = 0
  }

  def withX(x: Int) = Cursor(x, y)
  def withY(y: Int) = Cursor(x, y)
  def withRelativePos(deltaX: Int, deltaY: Int) = Cursor(x + deltaX, y + deltaY)
  def withRelativeX(deltaX: Int) = Cursor(x + deltaX, y)
  def withRelativeY(deltaY: Int) = Cursor(x, y + deltaY)
  override def compare(that: Cursor): Int = position - that.position
}


case class InputState(cord: Cord, linePositions: List[Int], position: Int)

case class InputBuffer(maxHistorySize: Int, tabSize: Int, private val cord: Cord = Cord.empty) {

  private val systemClipboard: Clipboard = Toolkit.getDefaultToolkit.getSystemClipboard


  private val history = RedoHistory(maxHistorySize)

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

  def ++=(str: String): Unit = str.foreach(this += _)
  def +=(char: Char): Unit = {
    if (char == null)
      return

    if (char == '\t') {
      val numSpaces = tabSize - (mainCursor.x % tabSize)
      this ++= " " * numSpaces
      return
    }

    upDownX = 0
    val text = currentCord
    val position = mainCursor.position
    if (position != text.length)
      linePositions = linePositions.map { pos => if (pos > position) pos + 1 else pos }

    if (char == '\n') {
      linePositions ::= position + 1
      linePositions = linePositions.sortWith(_ >= _)
    }

    val newValue = if (text.isEmpty || position == text.length) {
      text :- char
    } else if (position == 0) {
      text.-:(char)
    } else {
      val (before, after) = text.split(position)
      (before :- char) ++ after
    }
    setPos(mainCursor, position + 1)
    setPos(mark, position + 1)
    history += InputState(newValue, linePositions, position)
  }

  def paste(): Unit = {
    this ++= systemClipboard.getData(DataFlavor.stringFlavor).asInstanceOf[String]
  }

  def copy(): Unit = {
    val selected = if (mainCursor == mark) {
      val (start, end) = startAndEndOfLine
      currentCord.slice(start, end)
    } else {
      val cursors = List(mainCursor, mark).sorted
      currentCord.slice(cursors(0).position, cursors(1).position)
    }

    val selection = new StringSelection(selected.toString)
    systemClipboard.setContents(selection, selection)
  }

  def cut(): Unit = {
    if (mainCursor == mark)
      return
    copy()
    remove()
  }

  def remove(): Unit = {
    upDownX = 0
    val text = currentCord


    if (text.isEmpty)
      return

    val position = mainCursor.position

    val (newValue, newCursorPos) = if (mainCursor == mark) {
      val v = if (position == text.length) {
        text.init
      } else if (position == 1) {
        text.tail
      } else {
        val (before, after) = text.split(position)
        before.init ++ after
      }
      linePositions = linePositions.filter(_ != math.max(1, position))
      (v, position - 1)
    } else {
      val start = math.min(mainCursor.position, mark.position)
      val end = math.max(mainCursor.position, mark.position)

      // Filter linepositions in the removed area but don't remove the initial line
      linePositions = linePositions.filter(pos => !(pos in (math.max(1, start) to end)))

      val (before, _) = text.split(start)
      val (_, after) = text.split(end)
      (before ++ after, start)
    }

    val charsRemoved = text.length - newValue.length

    // Realign line positions
    linePositions = linePositions.map { pos => if (pos > position) pos - charsRemoved else pos }

    val cursorPos = math.min(math.max(newCursorPos, 0), currentCord.length)

    setPos(mainCursor, cursorPos)
    setPos(mark, cursorPos)
    history += InputState(newValue, linePositions, cursorPos)
  }

  def removeToLeftWord(): Unit = {
    val left = leftPosition
    //noinspection LoopVariableNotUpdated
    val position = mainCursor.position

    while (position != left)
      remove()
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

  def undo(): Boolean = _changeState(history.undo)
  def redo(): Boolean = _changeState(history.redo)


  def reset(cord: Cord = Cord.empty): Unit = {
    mainCursor.reset()
    upDownX = 0
    linePositions = List(0)
    linePositions ++= getLinePositions(cord)
    linePositions = linePositions.sortWith(_ >= _)
    history.clear()
    history += InputState(cord, linePositions, 0)
  }

  override def toString: String = currentCord.toString


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

  def debugString: String = "Lines: " + linePositions + "\n" + history

  private def startAndEndOfLine = {
    val line = lineIndex
    val end = if (line == 0)
      currentCord.length
    else
      linePositions(lineIndex - 1)

    (linePositions(line), end)
  }

  private def linePosition(line: Int): (Int, Int) = {
    val start = linePositions(line)
    val end = if (line == 0) history.current.cord.length else linePositions(line - 1) - 1
    (start, end)
  }

  private def lineIndex = linePositions.indices.find(i => linePositions(i) <= mainCursor.position).getOrElse(0)

  private def _changeState(f: () => Boolean): Boolean = {
    if (!f())
      return false

    upDownX = 0
    val newState = history.current
    setPos(mainCursor, newState.position)
    linePositions = newState.linePositions
    true
  }


}
