package tlang.utils

trait Positioned {
  private var _source   : Option[Source] = None
  private var _lineStart: Int            = 0
  private var _colStart : Int            = 0
  private var _lineEnd  : Int            = 0
  private var _colEnd   : Int            = 0

  def setPos(source: Source, lineStart: Int, colStart: Int, lineEnd: Int, colEnd: Int): this.type = {
    _lineStart = lineStart
    _colStart = colStart

    _lineEnd = lineEnd
    _colEnd = colEnd
    _source = Some(source)

    this
  }

  def setPos(other: Positioned): this.type = {
    _lineStart = other._lineStart
    _colStart = other._colStart

    _lineEnd = other._lineEnd
    _colEnd = other._colEnd

    _source = other._source


    this
  }

  def setNoPos(): this.type = {
    _lineStart = -1
    _colStart = -1

    _lineEnd = -1
    _colEnd = -1
    this
  }

  def setPos(start: Positioned, end: Positioned): this.type = {
    _lineStart = start._lineStart
    _colStart = start._colStart

    _lineEnd = end._lineEnd
    _colEnd = end._colEnd

    _source = start._source

    this
  }

  def source: Option[Source] = _source
  def line: Int = _lineStart
  def col: Int = _colStart
  def endLine: Int = _lineEnd
  def endCol: Int = _colEnd

  def encodedStartPos: Int = (line << 16) + col
  def encodedEndPos: Int = (endLine << 16) + endCol

  def isWithin(position: Positioned): Boolean =
    encodedStartPos >= position.encodedStartPos && encodedEndPos <= position.encodedEndPos

  def sourceName: String = source.map(_.mainName).getOrElse("Missing Source")

}

object NoPosition extends Position(-1, -1, -1, -1)

object Position {
  def apply(pos: Positioned): Position = Position(pos.line, pos.col, pos.endLine, pos.endCol)
}
case class Position(
  override val line: Int,
  override val col: Int,
  override val endLine: Int,
  override val endCol: Int)
  extends Positioned

case class PositionWithSource(
  override val line: Int,
  override val col: Int,
  override val endLine: Int,
  override val endCol: Int,
  override val source: Option[Source] = None)
  extends Positioned