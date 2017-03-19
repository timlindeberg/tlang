package tlang.repl

import scalaz.Cord

/**
  * Created by Tim Lindeberg on 3/19/2017.
  */
object CordExtensions {

  implicit class CordExtension(val cord: Cord) extends AnyVal {
    def iterator: Iterator[Char] = cord.self.iterator.flatMap(_.iterator)
    def reverseIterator: Iterator[Char] = cord.self.reverseIterator.flatMap(_.iterator)
    def slice(start: Int, end: Int): Cord = cord.split(start)._2.split(end)._1
  }

}
