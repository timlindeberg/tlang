package tlang.repl

/**
  * Created by Tim Lindeberg on 2/26/2017.
  */
case class CommandBuffer(maxHistorySize: Int, tabSize: Int) {
  /*
    val historicCommands: IndexedSeq[Cord] = {
      val list = ListBuffer[Cord]()
      val sb = new StringBuilder
      using(io.Source.fromFile(historyFile)) {
        _.getLines().foreach { line =>
          if (line.isEmpty) {
            list += Cord.empty :+ sb.toString
            sb.clear()
          } else {
            sb ++= line
          }
        }
      }
      list.toIndexedSeq
    }
  */
  private val history = History(maxHistorySize)

  private var linePositions: List[Int] = _
  private var position     : Int       = _
  private var upDownX      : Int       = _

  def x: Int = _x
  private def x_=(v: Int) = _x = v
  def y: Int = _y
  private def y_=(v: Int) = _y = v

  def lines: Int = linePositions.size

  private var _x: Int = _
  private var _y: Int = _

  clear()

  def ++=(str: String): Unit = str.foreach(this += _)
  def +=(char: Char): Unit = {

    if (char == '\t') {
      val numSpaces = tabSize - (x % tabSize)
      this ++= " " * numSpaces
      return
    }

    upDownX = 0
    val cord = history.current.cord
    if (position != cord.length)
      linePositions = linePositions.map { pos => if (pos > position) pos + 1 else pos }

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
    setPos(position + 1)
    history += State(newValue, linePositions, position)
  }

  def remove(): Unit = {
    upDownX = 0
    val cord = history.current.cord

    if (cord.isEmpty || position == 0)
      return

    if (position != cord.length)
      linePositions = linePositions.map { pos => if (pos > position) pos - 1 else pos }

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

    setPos(position - 1)
    history += State(newValue, linePositions, position)
  }

  def leftWord(): Unit = {
    val cord = history.current.cord
    if (cord.isEmpty || position == 0)
      return

    // Returns iterator string but the strings only contain one char
    // Reverse iterator to go left from the current pos
    val leftOf = cord.split(position)._1
    val it = leftOf.self.reverseIterator.map(_.head)
    val untilStop = charsUntilStop(it)
    setPos(if (untilStop == -1) 0 else position - untilStop)
  }

  def rightWord(): Unit = {
    val cord = history.current.cord
    if (cord.isEmpty || position == cord.length)
      return

    val it = cord.split(position)._2.self.iterator.map(_.head)
    val untilStop = charsUntilStop(it)
    setPos(if (untilStop == -1) cord.length else position + untilStop)
  }

  private def charsUntilStop(it: Iterator[Char]): Int = {
    val cord = history.current.cord
    val currentChar = cord(math.max(0, position - 1))

    val currentIsWhiteSpace = currentChar.isWhitespace
    var i = 0
    while (it.hasNext) {
      val c = it.next()
      if (currentIsWhiteSpace != c.isWhitespace) return i
      i += 1
    }
    -1
  }

  def left(count: Int): Unit = {
    upDownX = 0
    setPos(math.max(position - count, 0))
  }

  def right(count: Int): Unit = {
    upDownX = 0
    setPos(math.min(position + count, history.current.cord.size))
  }

  def up(): Unit = {
    if (upDownX == 0)
      upDownX = x

    val currentLine = lineIndex
    if (currentLine == linePositions.length - 1)
      return

    val (nextStart, nextEnd) = linePosition(currentLine + 1)
    setPos(nextStart + math.min(upDownX, nextEnd - nextStart))
  }

  def down(): Unit = {
    if (upDownX == 0)
      upDownX = x

    val currentLine = lineIndex
    if (currentLine == 0)
      return

    val (nextStart, nextEnd) = linePosition(currentLine - 1)
    setPos(nextStart + math.min(upDownX, nextEnd - nextStart))
  }

  def undo(): Boolean = _changeState(history.undo)
  def redo(): Boolean = _changeState(history.redo)


  def clear(): Unit = {
    position = 0
    x = 0
    y = 0
    upDownX = 0
    linePositions = List(0)
    history.clear()
    history += State.Empty
  }

  def command: String = history.current.cord.toString


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

  private def lineIndex = linePositions.indices.find(i => linePositions(i) <= position).get

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
