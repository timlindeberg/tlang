package tlang.utils

trait Enumerable[T] extends Traversable[T] {

  protected def All: List[T]
  override def foreach[U](f: T => U): Unit = All.foreach(f)

}