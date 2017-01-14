package tcompiler
package utils

import java.io.File

trait Positioned {
  private[Positioned] var _file     : Option[File] = None
  private[Positioned] var _lineStart: Int          = 0
  private[Positioned] var _colStart : Int          = 0
  private[Positioned] var _lineEnd  : Int          = 0
  private[Positioned] var _colEnd   : Int          = 0

  def setPos(file: File, start: Int, end: Int): this.type = {
    _lineStart = Position.line(start)
    _colStart = Position.column(start)

    _lineEnd = Position.line(end)
    _colEnd = Position.column(end)
    _file = Some(file)

    this
  }

  def hasPosition: Boolean = _file.isDefined

  def setPos(other: Positioned): this.type = {
    _lineStart = other._lineStart
    _colStart = other._colStart
    _file = other._file

    _lineEnd = other.endLine
    _colEnd = other.endCol

    this
  }

  def setPos(start: Positioned, end: Positioned): this.type = {
    _lineStart = start._lineStart
    _colStart = start._colStart
    _file = start._file

    _lineEnd = end._lineStart
    _colEnd = end._colStart

    this
  }

  def file: File = _file.get
  def line: Int = _lineStart
  def col: Int = _colStart
  def endLine: Int = _lineEnd
  def endCol: Int = _colEnd

  def position: String =
    if (hasPosition)
      s"${file.getPath}:$line:$col"
    else
      "?:?"

}

case object NoPosition extends Positioned
