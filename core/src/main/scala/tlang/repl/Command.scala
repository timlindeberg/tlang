package tlang.repl

import tlang.utils.Extensions._

import scalaz.Cord

object Cursor {
  def apply(x: Int, y: Int): Cursor = Cursor(0, x, y)
  def apply(): Cursor = Cursor(0, 0, 0)

}
case class Cursor(var position: Int, var x: Int, var y: Int) {

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
}

case class Position(mainCursor: Cursor, secondaryCursor: Cursor) {
  def isWithin(x: Int, y: Int): Boolean = {
    val minX = math.min(mainCursor.x, secondaryCursor.x)
    val maxX = math.max(mainCursor.x, secondaryCursor.x)
    val minY = math.min(mainCursor.y, secondaryCursor.y)
    val maxY = math.max(mainCursor.y, secondaryCursor.y)
    (x in (minX until maxX)) && (y in (minY to maxY))
  }

}

object NoPosition extends Position(Cursor(), Cursor()) {
  override def isWithin(x: Int, y: Int): Boolean = false
}

case class Command(maxHistorySize: Int, tabSize: Int, private val cord: Cord = Cord.empty) {

  private val history = RedoBuffer(maxHistorySize)

  private var linePositions: List[Int] = _
  private var upDownX      : Int       = _

  val mainCursor      = Cursor()
  val secondaryCursor = Cursor()

  def getPosition = Position(mainCursor, secondaryCursor)

  def height: Int = linePositions.length

  def currentCord: Cord = history.current.cord

  reset(cord)

  def lines: Int = linePositions.size

  def ++=(str: String): Unit = str.foreach(this += _)
  def +=(char: Char): Unit = {

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
    setPos(secondaryCursor, position + 1)
    history += State(newValue, linePositions, position)
  }

  def remove(): Unit = {
    upDownX = 0
    val text = currentCord


    if (text.isEmpty)
      return

    val position = mainCursor.position

    val (newValue, newCursorPos) = if (mainCursor == secondaryCursor) {
      val v = if (position == text.length) {
        text.init
      } else if (position == 1) {
        text.tail
      } else {
        val (before, after) = text.split(position)
        before.init ++ after
      }
      (v, position - 1)
    } else {
      val start = math.min(mainCursor.position, secondaryCursor.position)
      val end = math.max(mainCursor.position, secondaryCursor.position)
      val (before, _) = text.split(start)
      val (_, after) = text.split(end)
      (before ++ after, start)
    }

    val charsRemoved = text.length - newValue.length
    // Realign line positions
    linePositions = linePositions.map { pos => if (pos > position) pos - charsRemoved else pos }

    if (text(position - 1) == '\n')
      linePositions = linePositions.filter(_ != position)

    setPos(mainCursor, newCursorPos)
    setPos(secondaryCursor, newCursorPos)
    history += State(newValue, linePositions, newCursorPos)
  }

  def removeToLeftWord(): Unit = {
    val left = leftPosition
    //noinspection LoopVariableNotUpdated
    val position = mainCursor.position

    while (position != left)
      remove()
  }

  def setSecondaryCursorPosition(): Unit = {
    secondaryCursor.position = mainCursor.position
    secondaryCursor.x = mainCursor.x
    secondaryCursor.y = mainCursor.y
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
    history += State(cord, linePositions, 0)
  }

  def text: String = currentCord.toString


  private def leftPosition: Int = {
    val text = currentCord
    val position = mainCursor.position

    if (text.isEmpty || position == 0)
      return position

    val leftOf = text.split(position)._1
    val it = leftOf.self.reverseIterator
    val untilStop = charsUntilStop(it, position - 1)
    if (untilStop == -1) 0 else position - untilStop
  }

  private def rightPosition: Int = {
    val text = currentCord
    val position = mainCursor.position

    if (text.isEmpty || position == text.length)
      return position

    val rightOf = text.split(position)._2
    val it = rightOf.self.iterator
    val untilStop = charsUntilStop(it, position)
    if (untilStop == -1) text.length else position + untilStop
  }

  private def charsUntilStop(iterator: Iterator[String], pos: Int): Int = {
    val currentChar = currentCord(math.max(0, pos))

    val currentIsWhiteSpace = currentChar.isWhitespace
    var i = 0
    val it = iterator.flatMap(_.iterator)
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

  override def toString: String = "Lines: " + linePositions + "\n" + history

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
