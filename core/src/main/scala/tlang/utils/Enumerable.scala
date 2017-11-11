package tlang.utils

trait Enumerable[T] extends Seq[T] {

  protected def All: List[T]

  override def foreach[U](f: T => U): Unit = All.foreach(f)
  override def length: Int = All.length
  override def apply(idx: Int): T = All(idx)
  override def iterator: Iterator[T] = All.iterator
}
