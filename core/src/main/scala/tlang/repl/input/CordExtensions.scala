package tlang.repl.input

import scalaz.Cord

object CordExtensions {

  implicit class CordExtension(val cord: Cord) extends AnyVal {
    def iterator: Iterator[Char] = cord.self.iterator.flatMap(_.iterator)
    def reverseIterator: Iterator[Char] = cord.self.reverseIterator.flatMap(_.iterator)
    def slice(start: Int, end: Int): Cord = {
      val (a, b) = cord.split(start)
      b.split(end - a.length)._1
    }
  }

}
