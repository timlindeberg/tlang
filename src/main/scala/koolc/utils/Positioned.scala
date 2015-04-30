package koolc
package utils

import java.io.File

trait Positioned {
  private[Positioned] var _file: Option[File] = None
  private[Positioned] var _line: Int = 0
  private[Positioned] var _col: Int = 0

  def setPos(file: File, pos: Int): this.type = {
    _line = Position.line(pos)
    _col = Position.column(pos)
    _file = Some(file)

    this
  }

  def hasPosition = _file.isDefined

  def setPos(other: Positioned): this.type = {
    _line = other._line
    _col = other._col
    _file = other._file

    this
  }

  def file = _file.get
  def line = _line
  def col = _col

  def position: String = {
    if (hasPosition) {
      file.getPath + ":" + line + ":" + col
    } else {
      "?:?"
    }
  }
}

case object NoPosition extends Positioned
