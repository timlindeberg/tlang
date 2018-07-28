package tlang.utils

import tlang.formatting.{Formatter, SimpleFormatting}
import tlang.utils.Extensions._

trait Positioned {
  var source : Option[Source] = None
  var line   : Int            = 0
  var col    : Int            = 0
  var lineEnd: Int            = 0
  var colEnd : Int            = 0

  def setPos(source: Source, lineStart: Int, colStart: Int, lineEnd: Int, colEnd: Int): this.type = {
    this.line = lineStart
    this.col = colStart

    this.lineEnd = lineEnd
    this.colEnd = colEnd

    this.source = Some(source)

    this
  }

  def setPos(other: Positioned): this.type = {
    line = other.line
    col = other.col

    lineEnd = other.lineEnd
    colEnd = other.colEnd

    source = other.source

    this
  }

  def setNoPos(): this.type = {
    line = -1
    col = -1

    lineEnd = -1
    colEnd = -1
    this
  }

  def setPos(start: Positioned, end: Positioned): this.type = {
    line = start.line
    col = start.col

    lineEnd = end.lineEnd
    colEnd = end.colEnd

    source = start.source

    this
  }

  //  def line: Int = _lineStart
  //  def col: Int = _colStart
  //  def endLine: Int = _lineEnd
  //  def endCol: Int = _colEnd

  def encodedStartPos: Int = (line << 16) + col
  def encodedEndPos: Int = (lineEnd << 16) + colEnd

  def isWithin(position: Positioned): Boolean =
    encodedStartPos >= position.encodedStartPos && encodedEndPos <= position.encodedEndPos

  def simpleSourceDescription: String = sourceDescription(Formatter(SimpleFormatting))
  def sourceDescription(implicit formatter: Formatter): String = {
    import formatter.formatting._
    source.map(_.description).getOrElse(Red("Missing Source"))
  }

}

object NoPosition extends Position {
  setNoPos()
}

object Position {

  def apply(line: Int, col: Int, endLine: Int, endCol: Int): Position =
    apply(line, col, endLine, endCol, None)

  def apply(pos: Positioned): Position =
    apply(pos.line, pos.col, pos.lineEnd, pos.colEnd, pos.source)

  def apply(start: Positioned, end: Positioned): Position =
    apply(start.line, start.col, end.lineEnd, end.colEnd, start.source)

  def apply(line: Int, col: Int, endLine: Int, endCol: Int, source: Option[Source]): Position = {
    new Position use { p =>
      p.line = line
      p.col = col
      p.lineEnd = endLine
      p.colEnd = endCol
      p.source = source
    }
  }

  def unapply(pos: Position) = Some(pos.line, pos.col, pos.lineEnd, pos.colEnd)
}

class Position extends Positioned {
  override def equals(obj: scala.Any): Boolean = obj match {
    case Position(line, col, endLine, endCole) =>
      this.line == line && this.col == col && this.lineEnd == endLine && this.colEnd == endCole
    case _                                     => false
  }

  override def hashCode(): Int = {
    var hash = 17
    hash = hash * 31 + line
    hash = hash * 31 + col
    hash = hash * 31 + lineEnd
    hash = hash * 31 + colEnd
    hash
  }

  override def toString: String = s"Position($line, $col, $lineEnd, $colEnd)"
}
