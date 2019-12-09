package tlang
package repl
package terminal

import com.googlecode.lanterna.TerminalPosition

object TerminalPositionExtensions {

  implicit class TerminalPositionExtensions(val pos: TerminalPosition) extends AnyVal {

    def x: Int = pos.getColumn
    def y: Int = pos.getRow

    def +(other: TerminalPosition) = new TerminalPosition(x + pos.x, y + pos.y)
    def -(other: TerminalPosition) = new TerminalPosition(x - pos.x, y - pos.y)
  }

}
