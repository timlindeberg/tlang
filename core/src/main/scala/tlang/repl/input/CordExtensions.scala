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

    def equalCord(other: Cord): Boolean = {
      if (cord.length != other.length)
        return false

      val it1 = cord.self.iterator.flatMap(_.iterator)
      val it2 = other.self.iterator.flatMap(_.iterator)

      while (it1.hasNext)
        if (it1.next != it2.next)
          return false
      true
    }
  }

}
