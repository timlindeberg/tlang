package tlang.repl.input

object Cursor {
  def apply(x: Int, y: Int): Cursor = Cursor(0, x, y)
  def apply(): Cursor = Cursor(0, 0, 0)

}

case class Selection(main: Cursor, mark: Cursor)

case class Cursor(position: Int, x: Int, y: Int) extends Ordered[Cursor] {

  def getPosition: (Int, Int) = (x, y)

  def withX(x: Int): Cursor = Cursor(x, y)
  def withY(y: Int): Cursor = Cursor(x, y)
  def withRelativePos(deltaX: Int, deltaY: Int): Cursor = Cursor(x + deltaX, y + deltaY)
  def withRelativeX(deltaX: Int): Cursor = Cursor(x + deltaX, y)
  def withRelativeY(deltaY: Int): Cursor = Cursor(x, y + deltaY)

  override def compare(that: Cursor): Int = position - that.position
  override def equals(any: Any): Boolean = any match {
    case Cursor(pos, _, _) => pos == position
    case _                 => false
  }
}

