package tlang
package repl
package input

import scalaz.Cord
import tlang.formatting.Colors
import tlang.repl.input.CordExtensions._
import tlang.utils.Position

import scala.collection.mutable.ListBuffer

object InputBuffer {

  def calculateLinePositions(string: String): List[Int] = {
    val lines = ListBuffer[Int]()
    var i = 1
    string foreach { c =>
      if (c == '\n') lines += i
      i += 1
    }

    lines.toList
  }

  def apply(tabWidth: Int): InputBuffer = {
    InputBuffer(tabWidth, Cord.empty :+ "", Cursor(), Cursor(), 0 :: Nil)
  }

  def apply(str: String, tabWidth: Int): InputBuffer = {
    val s = str.withUnixLineEndings
    val linePositions = (0 :: calculateLinePositions(s)).reverse
    InputBuffer(tabWidth, Cord.empty :+ s, Cursor(), Cursor(), linePositions)
  }
}

case class InputBuffer(
  tabWidth: Int,
  cord: Cord,
  mainCursor: Cursor,
  secondaryCursor: Cursor,
  private val linePositions: List[Int],
  private val upDownX: Int = 0) {

  import InputBuffer._

  def height: Int = linePositions.length

  def isEmpty: Boolean = cord.isEmpty

  def +(char: Char): InputBuffer = add(char)
  def add(char: Char): InputBuffer = addString(s"$char")
  def ++(str: String): InputBuffer = add(str)
  def add(str: String): InputBuffer = addString(str.withUnixLineEndings)

  def removeSelected(): InputBuffer = {
    if (isEmpty)
      return this

    val (start, end) = selection
    var newLinePositions = linePositions

    val (newCord, newCursorPos) = if (start == end) {
      // If the char to remove is a newline, remove the position from the line positions
      newLinePositions = newLinePositions.filter(_ != math.max(1, start))
      (cord.removeCharAt(start), start - 1)
    } else {
      // Filter line positions in the removed area but don't remove the initial line
      val removedRange = math.max(1, start) until end
      newLinePositions = newLinePositions.filter(pos => !(pos - 1 in removedRange))
      (cord.removeBetween(start, end), start)
    }

    val charsRemoved = cord.length - newCord.length

    // Realign line positions
    newLinePositions = newLinePositions.map { pos => if (pos > start) pos - charsRemoved else pos }

    val newCursor = getCursor(newCursorPos.clamp(0, cord.length), newLinePositions)
    InputBuffer(tabWidth, newCord, newCursor, newCursor, newLinePositions)
  }

  def moveCursorHorizontal(steps: Int, moveSecondary: Boolean = true): InputBuffer = {
    moveCursor((mainCursor.position + steps).clamp(0, cord.size), moveSecondary)
  }

  def moveCursorVertical(steps: Int, moveSecondary: Boolean = true): InputBuffer = {
    val lineIndex = currentLineIndex
    val nextLine = (lineIndex + steps).clamp(0, linePositions.length - 1)
    if (nextLine == lineIndex)
      return this

    val x = if (upDownX == 0) mainCursor.x else upDownX
    val y = linePositions.length - nextLine - 1
    moveCursor(x, y, moveSecondary, x)
  }

  def moveCursorToLeftWord(moveSecondary: Boolean = true): InputBuffer = {
    if (cord.isEmpty)
      return this

    val pos = math.max(0, mainCursor.position - 1)
    val isWhitespaceAtNextChar = cord(pos).isWhitespace
    moveCursor(leftWhere { _.isWhitespace != isWhitespaceAtNextChar }, moveSecondary)
  }

  def moveCursorToRightWord(moveSecondary: Boolean = true): InputBuffer = {
    if (cord.isEmpty)
      return this

    val pos = math.min(cord.length - 1, mainCursor.position)
    val isWhitespaceAtNextChar = cord(pos).isWhitespace
    moveCursor(rightWhere { _.isWhitespace != isWhitespaceAtNextChar }, moveSecondary)
  }

  def selectCurrentLine(selectNewLine: Boolean = false): InputBuffer = {
    val (start, end) = currentLinePosition
    val secondary = getCursor(start)
    val main = getCursor(if (selectNewLine) math.min(end + 1, cord.length) else end)

    copy(mainCursor = main, secondaryCursor = secondary)
  }

  def selectCurrentWord(): InputBuffer = {
    val stop: Char => Boolean = c => c in " \t\n=().,_-"
    val start = leftWhere(stop)
    val end = rightWhere(stop)
    copy(mainCursor = getCursor(end), secondaryCursor = getCursor(start))
  }

  def selected: String = {
    if (mainCursor == secondaryCursor)
      return ""

    cord.slice(selection).toString.withSystemLineEndings
  }

  def currentLinePosition: (Int, Int) = linePosition(currentLineIndex)
  def currentLine: String = cord.slice(currentLinePosition).toString

  def selectedPosition: Position = {
    val (first, second) = orderedCursors
    Position(first.y + 1, first.x + 1, second.y + 1, second.x + 1)
  }

  override def toString: String = cord.toString.withSystemLineEndings

  def debugString: String = {
    val s = cord.toString
    val mainMarker = Colors.Red("|")
    val secondaryMarker = Colors.Blue("|")

    val text = if (mainCursor == secondaryCursor)
      s.insert(mainMarker, mainCursor.position)
    else if (mainCursor.position > secondaryCursor.position)
      s.insert(mainMarker, mainCursor.position).insert(secondaryMarker, secondaryCursor.position)
    else
      s.insert(secondaryMarker, secondaryCursor.position).insert(mainMarker, mainCursor.position)

    s"""
       |${ text.escape(EscapeCharsNormal) }
       |   Lines:   $linePositions
       |   Cursor:  $mainCursor
       |   Mark:    $secondaryCursor
       |   UpDownX: $upDownX
     """.stripMargin.trim
  }

  private def selection: (Int, Int) = orderedCursors.map(_.position)

  private def orderedCursors: (Cursor, Cursor) =
    if (mainCursor > secondaryCursor)
      (secondaryCursor, mainCursor)
    else
      (mainCursor, secondaryCursor)

  private def addString(str: String): InputBuffer = {
    val buffer = if (mainCursor != secondaryCursor) removeSelected() else this
    buffer._addString(str)
  }

  private def _addString(str: String): InputBuffer = {
    val position = mainCursor.position
    var newLinePositions = linePositions

    // If the current position is before the last line number we have to adjust them
    if (position < newLinePositions.head)
      newLinePositions = newLinePositions.map { pos => if (pos > position) pos + str.length else pos }

    val linePositionsInNewChars = calculateLinePositions(str).map(_ + position)
    if (linePositionsInNewChars.nonEmpty)
      newLinePositions = (newLinePositions ::: linePositionsInNewChars).sortWith(_ >= _)

    val newCord = cord.addAtIndex(position, str)

    val buffer = copy(cord = newCord)
    val newMainCursor = buffer.getCursor(position + str.length, newLinePositions)
    InputBuffer(tabWidth, newCord, newMainCursor, newMainCursor, newLinePositions)
  }

  def moveCursor(x: Int, y: Int): InputBuffer = moveCursor(x, y, moveSecondary = true, 0)
  def moveCursor(x: Int, y: Int, moveSecondary: Boolean): InputBuffer = moveCursor(x, y, moveSecondary, 0)
  def moveCursor(x: Int, y: Int, moveSecondary: Boolean, upDownX: Int): InputBuffer = {
    moveCursor(innerRepresentation(x, y), moveSecondary, upDownX)
  }

  def moveCursor(position: Int): InputBuffer = moveCursor(position, moveSecondary = true, 0)
  def moveCursor(position: Int, moveSecondary: Boolean): InputBuffer = moveCursor(position, moveSecondary, 0)
  def moveCursor(position: Int, moveSecondary: Boolean, upDownX: Int): InputBuffer = {
    if (position == mainCursor.position && (!moveSecondary || secondaryCursor == mainCursor))
      return this

    val newMainCursor = getCursor(position)
    copy(
      mainCursor = newMainCursor,
      secondaryCursor = if (moveSecondary) newMainCursor else secondaryCursor,
      upDownX = upDownX
    )
  }

  private def outerRepresentation(pos: Int, linePositions: List[Int] = linePositions): (Int, Int) = {
    val index = lineIndex(pos, linePositions)
    val lineStart = linePositions(index)
    val x = pos - lineStart
    val y = linePositions.length - index - 1

    val offset = tabOffset(lineStart, pos)
    (x + offset, y)
  }

  private def innerRepresentation(x: Int, y: Int): Int = {
    val (lineStart, lineEnd) = linePosition(linePositions.length - y - 1)
    lineStart + adjustXPosition(x, lineStart, lineEnd)
  }

  private def adjustXPosition(x: Int, lineStart: Int, lineEnd: Int): Int = {
    val slice = cord.slice(lineStart, lineEnd)

    var chars = 0
    var tabOffset = 0
    for (c <- slice.iterator) {
      if (c == '\t')
        tabOffset += tabWidth - 1
      if (chars + tabOffset >= x) {
        // If x is closer to the end of the tab than the start, add 1
        val xPercentageOfTab = (x % tabWidth).asInstanceOf[Double] / tabWidth
        return if (c == '\t' && math.round(xPercentageOfTab) == 1) chars + 1 else chars
      }

      chars += 1
    }
    chars
  }

  private def tabOffset(start: Int, end: Int): Int = {
    val slice = cord.slice(start, end)
    slice.iterator.count(_ == '\t') * (tabWidth - 1)
  }

  private def leftWhere(stop: Char => Boolean): Int = {
    val position = mainCursor.position

    if (isEmpty || position == 0)
      return position

    val leftOf = cord.split(position)._1
    charsUntilStop(leftOf.reverseIterator, position - 1, stop) match {
      case -1       => 0
      case numChars => position - numChars
    }
  }

  private def rightWhere(stop: Char => Boolean): Int = {
    val position = mainCursor.position

    if (isEmpty || position == cord.length)
      return position

    val rightOf = cord.split(position)._2
    charsUntilStop(rightOf.iterator, position, stop) match {
      case -1       => cord.length
      case numChars => position + numChars
    }
  }

  private def charsUntilStop(it: Iterator[Char], pos: Int, stop: Char => Boolean): Int = {
    var i = 0
    while (it.hasNext) {
      val c = it.next()
      if (stop(c))
        return i
      i += 1
    }
    -1
  }

  private def getCursor(pos: Int, linePositions: List[Int] = linePositions): Cursor = {
    val (x, y) = outerRepresentation(pos, linePositions)
    Cursor(pos, x, y)
  }

  private def linePosition(line: Int): (Int, Int) = {
    val start = linePositions(line)
    val end = if (line == 0) cord.length else linePositions(line - 1) - 1
    (start, end)
  }

  private def currentLineIndex = lineIndex(mainCursor.position, linePositions)
  private def lineIndex(position: Int, linePositions: List[Int]) =
    linePositions.indices.find(i => linePositions(i) <= position).getOrElse(0)
}
