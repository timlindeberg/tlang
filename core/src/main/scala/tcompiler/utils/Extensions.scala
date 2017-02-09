package tcompiler.utils

import tcompiler.analyzer.Types.Type

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.reflect.{ClassTag, _}
import scala.util.matching.Regex

/**
  * Created by Tim Lindeberg on 4/16/2016.
  */
object Extensions {

  private val AnsiRegex: Regex = """\x1b[^m]*m""".r

  def using[T <: {def close()}, R](resource: T)(block: T => R): R = {
    try {
      block(resource)
    } finally {
      if (resource != null) resource.close()
    }
  }

  implicit class OptionExtensions[T](val o: Option[T]) extends AnyVal {

    def ifDefined(f: T => Unit): Unit = if (o.isDefined) f(o.get)

  }

  implicit class IntExtensions(val i: Int) extends AnyVal {

    def times(f: => Unit): Unit = 1 to i foreach { _ => f }

  }

  implicit class RegexExtensions(r: Regex) {
    def matches(s: String): Boolean = r.pattern.matcher(s).matches
  }

  implicit class StringExtensions(val str: String) extends AnyVal {
    def isAnsi: Boolean = AnsiRegex.matches(str)
    def clearAnsi: String = AnsiRegex.replaceAllIn(str, "")
    def charCount: Int = {
      val str = clearAnsi
      str.codePointCount(0, str.length)
    }

    def takeChars(num: Int): String = {
      var i = 0
      var s = ""
      while (s.charCount < num) {
        s += str(i)
        i += 1
      }
      s
    }

    def trimWhiteSpaces: String = str.leftTrimWhiteSpaces.rightTrimWhiteSpaces
    def leftTrimWhiteSpaces: String = str.replaceAll("^\\s+", "")
    def rightTrimWhiteSpaces: String = str.replaceAll("\\s+$", "")

    def allIndexesOf(pattern: String): List[Int] = {
      val buf = ListBuffer[Int]()

      var index = str.indexOf(pattern)
      if (index != -1)
        buf += index
      while (index >= 0) {
        index = str.indexOf(pattern, index + 1)
        if (index != -1)
          buf += index
      }
      buf.toList
    }
  }

  implicit class AnyExtensions(val a: Any) extends AnyVal {

    def ifInstanceOf[T: ClassTag](f: T => Unit): Unit = if (classTag[T].runtimeClass.isInstance(a)) f(a.asInstanceOf[T])

  }

  implicit class GenericExtensions[T](val t: T) extends AnyVal {
    def use(f: T => Unit): T = {f(t); t}
    def print: T = {println(t); t}
    def in(seq: Traversable[T]): Boolean = seq.exists(_ == t)
    def in(seq: Set[T]): Boolean = seq.contains(t)
  }

  implicit class TypeTuple(val t: (Type, Type)) extends AnyVal {

    def anyIs(types: Type*): Boolean = types.map(_.getClass).exists(c => c == t._1.getClass || c == t._2.getClass)
    def bothAre(types: Type*): Boolean = types.map(_.getClass).exists(c => c == t._1.getClass && c == t._2.getClass)
  }

  implicit class TraversableExtensions[Collection[T] <: Traversable[T], T](val collection: Collection[T]) extends AnyVal {

    def filterInstance[A <: T : ClassTag]: Collection[A] = collection.filter(classTag[A].runtimeClass.isInstance(_)).asInstanceOf[Collection[A]]
    def filterNotInstance[A <: T : ClassTag]: Collection[T] = collection.filter(!classTag[A].runtimeClass.isInstance(_)).asInstanceOf[Collection[T]]
    def findInstance[A <: T : ClassTag]: Option[A] = collection.find(classTag[A].runtimeClass.isInstance(_)).asInstanceOf[Option[A]]
    def findDefined[A](f: T => Option[A]): Option[A] = {
      for (v <- collection) {
        val o = f(v)
        if (o.isDefined) return o
      }
      None
    }
  }

  implicit class MutableMapExtensions[K, V](val m: mutable.Map[K, V]) extends AnyVal {

    def getOrElseMaybeUpdate(key: K, op: => Option[V]): Option[V] =
      m.get(key) match {
        case Some(v) => Some(v)
        case None    => op match {
          case Some(v) =>
            m += key -> v
            Some(v)
          case None    => None
        }
      }

  }


}
