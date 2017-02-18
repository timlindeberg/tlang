package tlang.compiler

import tlang.compiler.utils.Positioned

/**
  * Created by Tim Lindeberg on 1/22/2017.
  */

case class Pos(line: Int, col: Int, endLine: Int, endCol: Int) {
  def this(pos: Positioned) = this(pos.line, pos.col, pos.endLine, pos.endCol)
}