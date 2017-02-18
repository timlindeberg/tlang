package tlang.compiler.utils

/**
  * Created by Tim Lindeberg on 2/18/2017.
  */
case class Position(override val line: Int, override val col: Int, override val endLine: Int, override val endCol: Int) extends Positioned
