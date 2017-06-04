package tlang.utils

object Position {
  val NoPos = Position(-1, -1, -1, -1)
}

case class Position(override val line: Int, override val col: Int, override val endLine: Int, override val endCol: Int)
  extends Positioned
