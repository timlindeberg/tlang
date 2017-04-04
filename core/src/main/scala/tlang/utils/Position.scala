package tlang.utils

case class Position(override val line: Int, override val col: Int, override val endLine: Int, override val endCol: Int)
  extends Positioned
