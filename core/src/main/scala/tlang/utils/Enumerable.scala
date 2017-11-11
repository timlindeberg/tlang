package tlang.utils

trait Enumerable[T] extends Seq[T] {

  protected def Values: List[T]

  override def foreach[U](f: T => U): Unit = Values.foreach(f)
  override def length: Int = Values.length
  override def apply(idx: Int): T = Values(idx)
  override def iterator: Iterator[T] = Values.iterator
}
