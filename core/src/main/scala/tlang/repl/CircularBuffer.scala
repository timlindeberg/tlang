package tlang.repl

import tlang.utils.Extensions._

import scala.collection.mutable.ArrayBuffer


object CircularBuffer {

  def apply[T](ts: T*): CircularBuffer[T] = new CircularBuffer[T]().use { b => ts.foreach(b += _) }

}
/**
  * Created by Tim Lindeberg on 3/4/2017.
  */
class CircularBuffer[T] extends Traversable[T] {

  private val v     = ArrayBuffer[T]()
  private var index = 0

  def +=(t: T): this.type = {v += t; this}
  def -=(t: T): this.type = {v -= t; this}

  def +=(i: Int): this.type = {index = mod(index + i, v.size); this}
  def -=(i: Int): this.type = {index = mod(index - i, v.size); this}

  def apply(): T = v(index)
  def apply(index: Int): T = v(index)

  def setIndex(i: Int): Unit = index = i

  private def mod(i: Int, m: Int): Int = {
    val x = i % m
    if (x < 0) x + m else x
  }
  override def foreach[U](f: (T) => U): Unit = v.foreach(f)
}
