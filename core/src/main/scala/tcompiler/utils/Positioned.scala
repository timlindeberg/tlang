package tcompiler
package utils

import java.io.File

trait Positioned {
  private var _file     : Option[File] = None
  private var _lineStart: Int          = 0
  private var _colStart : Int          = 0
  private var _lineEnd  : Int          = 0
  private var _colEnd   : Int          = 0

  def setPos(file: Option[File], lineStart: Int, colStart: Int, lineEnd: Int, colEnd: Int): this.type = {
    _lineStart = lineStart
    _colStart = colStart

    _lineEnd = lineEnd
    _colEnd = colEnd
    _file = file

    this
  }

  def hasFile: Boolean = _file.isDefined

  def setPos(other: Positioned): this.type = {
    _lineStart = other._lineStart
    _colStart = other._colStart

    _lineEnd = other._lineEnd
    _colEnd = other._colEnd

    _file = other._file


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

    _file = start._file

    this
  }

  def file: Option[File] = _file
  def line: Int = _lineStart
  def col: Int = _colStart
  def endLine: Int = _lineEnd
  def endCol: Int = _colEnd

  def position: String =
    if (hasFile)
      s"${file.get.getPath}:$line:$col"
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
