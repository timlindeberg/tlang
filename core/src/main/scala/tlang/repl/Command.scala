package tlang.repl

import scalaz.Cord

/**
  * Created by Tim Lindeberg on 2/26/2017.
  */
case class Command(maxHistorySize: Int, tabSize: Int, private val cord: Cord = Cord.empty) {

  private val history = RedoBuffer(maxHistorySize)

  private var linePositions: List[Int] = _
  private var position     : Int       = _
  private var upDownX      : Int       = _


  private var _x: Int = _
  private var _y: Int = _
  private def x_=(v: Int) = _x = v
  private def y_=(v: Int) = _y = v

  def height = linePositions.length

  def currentCord: Cord = history.current.cord

  reset(cord)

  def x: Int = _x
  def y: Int = _y
  def lines: Int = linePositions.size

  def ++=(str: String): Unit = str.foreach(this += _)
  def +=(char: Char): Unit = {

    if (char == '\t') {
      val numSpaces = tabSize - (x % tabSize)
      this ++= " " * numSpaces
      return
    }

    upDownX = 0
    val text = currentCord
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
    setPos(position + 1)
    history += State(newValue, linePositions, position)
  }

  def remove(): Unit = {
    upDownX = 0
    val text = currentCord

    if (text.isEmpty || position == 0)
      return

    if (position != text.length)
      linePositions = linePositions.map { pos => if (pos > position) pos - 1 else pos }

    if (text(position - 1) == '\n')
      linePositions = linePositions.filter(_ != position)


    val newValue = if (position == text.length) {
      text.init
    } else if (position == 1) {
      text.tail
    } else {
      val (before, after) = text.split(position)
      before.init ++ after
    }

    setPos(position - 1)
    history += State(newValue, linePositions, position)
  }

  def removeToLeftWord(): Unit = {
    val left = leftPosition
    //noinspection LoopVariableNotUpdated
    while (position != left)
      remove()
  }

  def goToLeftWord(): Unit = setPos(leftPosition)
  def goToRightWord(): Unit = setPos(rightPosition)

  def moveLeft(count: Int): Unit = {
    upDownX = 0
    setPos(math.max(position - count, 0))
  }

  def moveRight(count: Int): Unit = {
    upDownX = 0
    setPos(math.min(position + count, history.current.cord.size))
  }

  def up(): Boolean = {
    val currentLine = lineIndex
    if (currentLine == linePositions.length - 1)
      return false

    if (upDownX == 0)
      upDownX = x


    val (nextStart, nextEnd) = linePosition(currentLine + 1)
    setPos(nextStart + math.min(upDownX, nextEnd - nextStart))
    true
  }

  def down(): Boolean = {
    val currentLine = lineIndex
    if (currentLine == 0)
      return false

    if (upDownX == 0)
      upDownX = x

    val (nextStart, nextEnd) = linePosition(currentLine - 1)
    setPos(nextStart + math.min(upDownX, nextEnd - nextStart))
    true
  }

  def undo(): Boolean = _changeState(history.undo)
  def redo(): Boolean = _changeState(history.redo)


  def reset(cord: Cord = Cord.empty): Unit = {
    position = 0
    x = 0
    y = 0
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
    if (text.isEmpty || position == 0)
      return position

    val leftOf = text.split(position)._1
    val it = leftOf.self.reverseIterator
    val untilStop = charsUntilStop(it, position - 1)
    if (untilStop == -1) 0 else position - untilStop
  }

  private def rightPosition: Int = {
    val text = currentCord
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

  private def setPos(pos: Int): Unit = {
    position = pos
    val index = lineIndex
    x = position - linePositions(index)
    y = linePositions.length - index - 1
  }

  override def toString: String = {
    "Lines: " + linePositions + "\n" + history
  }

  private def linePosition(line: Int): (Int, Int) = {
    val start = linePositions(line)
    val end = if (line == 0) history.current.cord.length else linePositions(line - 1) - 1
    (start, end)
  }

  private def lineIndex = linePositions.indices.find(i => linePositions(i) <= position).getOrElse(0)

  private def _changeState(f: () => Boolean): Boolean = {
    if (!f())
      return false

    upDownX = 0
    val newState = history.current
    setPos(newState.position)
    linePositions = newState.linePositions
    true
  }


}
