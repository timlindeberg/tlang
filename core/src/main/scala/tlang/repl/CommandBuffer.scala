package tlang.repl

/**
  * Created by Tim Lindeberg on 2/26/2017.
  */
case class CommandBuffer(maxHistorySize: Int) {


  private val history = History(maxHistorySize)

  private var linePositions: List[Int] = _
  private var position     : Int       = _
  private var upDownX      : Int       = _

  clear()

  def ++=(str: String): Unit = str.foreach(this += _)
  def +=(char: Char): Unit = {
    upDownX = 0
    val cord = history.current.cord
    if (char == '\n') {
      linePositions ::= position + 1
      linePositions = linePositions.sortWith(_ >= _)
    }

    val newValue = if (cord.isEmpty || position == cord.length) {
      cord :- char
    } else if (position == 0) {
      cord.-:(char)
    } else {
      val (before, after) = cord.split(position)
      (before :- char) ++ after
    }
    position += 1
    history += State(newValue, linePositions, position)
  }

  def remove(): Unit = {
    upDownX = 0
    val cord = history.current.cord

    if (cord.isEmpty || position == 0)
      return

    if (cord(position - 1) == '\n')
      linePositions = linePositions.filter(_ != position)

    val newValue = if (position == cord.length) {
      cord.init
    } else if (position == 1) {
      cord.tail
    } else {
      val (before, after) = cord.split(position)
      before.init ++ after
    }
    position -= 1
    history += State(newValue, linePositions, position)
  }

  def left(count: Int): Unit = {
    upDownX = 0
    position = math.max(position - count, 0)
  }
  def right(count: Int): Unit = {
    upDownX = 0
    position = math.min(position + count, history.current.cord.size)
  }

  def up(): Unit = {
    if (upDownX == 0)
      upDownX = translatedPosition._2

    val currentLine = linePositionIndex
    if (currentLine == linePositions.length - 1)
      return

    val (nextStart, nextEnd) = linePosition(currentLine + 1)
    position = nextStart + math.min(upDownX, nextEnd - nextStart)
  }

  def down(): Unit = {
    if (upDownX == 0)
      upDownX = translatedPosition._2

    val currentLine = linePositionIndex
    if (currentLine == 0)
      return

    val (nextStart, nextEnd) = linePosition(currentLine - 1)
    position = nextStart + math.min(upDownX, nextEnd - nextStart)
  }

  def undo(): Boolean = _changeState(history.undo)
  def redo(): Boolean = _changeState(history.redo)


  def clear(): Unit = {
    position = 0
    upDownX = 0
    linePositions = List(0)
    history.clear()
    history += State.Empty
  }

  def command: String = history.current.cord.toString


  def translatedPosition: (Int, Int) = {
    val index = linePositionIndex
    (linePositions.length - index - 1, position - linePositions(index))
  }

  override def toString: String = {
    "Lines: " + linePositions + "\n" + history
  }

  private def linePosition(line: Int): (Int, Int) = {
    val start = linePositions(line)
    val end = if (line == 0) history.current.cord.length else linePositions(line - 1) - 1
    (start, end)
  }

  private def linePositionIndex = linePositions.indices.find(i => linePositions(i) <= position).get

  private def _changeState(f: () => Boolean): Boolean = {
    if (!f())
      return false

    upDownX = 0
    val newState = history.current
    position = newState.position
    linePositions = newState.linePositions
    true
  }


}
