package tlang.repl.input

import scalaz.Cord

object CordExtensions {

  implicit class CordExtension(val cord: Cord) extends AnyVal {
    def iterator: Iterator[Char] = cord.self.iterator.flatMap(_.iterator)
    def reverseIterator: Iterator[Char] = cord.self.reverseIterator.flatMap(_.reverseIterator)
    def slice(startEnd: (Int, Int)): Cord = slice(startEnd._1, startEnd._2)

    def slice(start: Int, end: Int): Cord = {
      if (start == end)
        return Cord.empty

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

    def addAtIndex(index: Int, str: String): Cord = {
      if (cord.isEmpty || index == cord.length)
        return cord :+ str

      if (index == 0)
        return cord.+:(str)

      val (before, after) = cord.split(index)
      (before :+ str) ++ after
    }

    def removeCharAt(index: Int): Cord = {
      if (index == cord.length)
        return cord.init

      if (index == 1)
        return cord.tail

      val (before, after) = cord.split(index)
      before.init ++ after
    }

    def removeBetween(start: Int, end: Int): Cord = {
      val (before, _) = cord.split(start)
      val (_, after) = cord.split(end)
      before ++ after
    }
  }

}
