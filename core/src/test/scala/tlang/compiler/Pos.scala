package tlang.compiler

import tlang.utils.Positioned

object Pos {
  def apply(pos: Positioned): Pos = new Pos(pos)
}
case class Pos(line: Int, col: Int, endLine: Int, endCol: Int) {
  def this(pos: Positioned) = this(pos.line, pos.col, pos.endLine, pos.endCol)
}