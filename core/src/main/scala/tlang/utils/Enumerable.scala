package tlang.utils

/**
  * Created by Tim Lindeberg on 3/5/2017.
  */

trait Enumerable[T] extends Traversable[T] {

  protected def All: Set[T]
  override def foreach[U](f: T => U): Unit = All.foreach(f)

}