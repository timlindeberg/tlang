package tlang
package utils


import scala.collection.mutable.ArrayBuffer

object CircularBuffer {
  def apply[T](ts: T*): CircularBuffer[T] = CircularBuffer[T]().use { _ ++= ts }
}

case class CircularBuffer[T]() extends ArrayBuffer[T] {

  def index: Int = _index
  private var _index = 0

  def advance(i: Int): this.type = { _index = mod(_index + i, size); this }

  def current: T = apply(_index)
  def setCurrent(elem: T): this.type = { update(_index, elem); this }

  def setPosition(i: Int): Unit = _index = i

  private def mod(i: Int, m: Int): Int = {
    val x = i % m
    if (x < 0) x + m else x
  }

}
