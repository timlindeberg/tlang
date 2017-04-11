package tlang.repl.input

object Cursor {
  def apply(x: Int, y: Int): Cursor = Cursor(0, x, y)
  def apply(): Cursor = Cursor(0, 0, 0)

}
case class Cursor(var position: Int, var x: Int, var y: Int) extends Ordered[Cursor] {

  def reset(): Unit = {
    position = 0
    x = 0
    y = 0
  }

  def withX(x: Int) = Cursor(x, y)
  def withY(y: Int) = Cursor(x, y)
  def withRelativePos(deltaX: Int, deltaY: Int) = Cursor(x + deltaX, y + deltaY)
  def withRelativeX(deltaX: Int) = Cursor(x + deltaX, y)
  def withRelativeY(deltaY: Int) = Cursor(x, y + deltaY)
  override def compare(that: Cursor): Int = position - that.position
  override def equals(any: Any): Boolean = any match {
    case Cursor(pos, _, _) => pos == position
    case _                 => false
  }
}

