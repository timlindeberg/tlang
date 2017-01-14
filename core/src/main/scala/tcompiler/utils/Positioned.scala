package tcompiler
package utils

import java.io.File

trait Positioned {
  private var _file     : Option[File] = None
  private var _lineStart: Int          = 0
  private var _colStart : Int          = 0
  private var _lineEnd  : Int          = 0
  private var _colEnd   : Int          = 0

  def setPos(file: File, lineStart: Int, colStart: Int, lineEnd: Int, colEnd: Int): this.type = {
    _lineStart = lineStart
    _colStart = colStart

    _lineEnd = lineEnd
    _colEnd = colEnd
    _file = Some(file)

    this
  }

  def hasPosition: Boolean = _file.isDefined

  def setPos(other: Positioned): this.type = {
    _lineStart = other._lineStart
    _colStart = other._colStart

    _lineEnd = other._lineEnd
    _colEnd = other._colEnd

    _file = other._file


    this
  }

  def setPos(start: Positioned, end: Positioned): this.type = {
    _lineStart = start._lineStart
    _colStart = start._colStart

    _lineEnd = end._lineStart
    _colEnd = end._colStart

    _file = start._file

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

  def equalPos(other: Positioned): Boolean =
    _file == other._file &&
      _lineStart == other._lineStart &&
      _colStart == other._colStart &&
      _lineEnd == other._lineEnd &&
      _colEnd == other._colEnd

}

case object NoPosition extends Positioned
