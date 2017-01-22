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

  val AnsiRegex: Regex = """\x1b[^m]*m""".r

  implicit class OptionExtensions[T](o: Option[T]) {

    def ifDefined(f: T => Unit): Unit = if (o.isDefined) f(o.get)

  }

  implicit class IntExtensions(i: Int) {

    def times(f: => Unit): Unit = 1 to i foreach { _ => f }

  }

  implicit class StringExtensions(str: String) {
    def clearAnsi: String = AnsiRegex.replaceAllIn(str, "")
    def charCount: Int = {
      val str = clearAnsi
      str.codePointCount(0, str.length)
    }

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

  implicit class AnyExtensions(a: Any) {

    def ifInstanceOf[T: ClassTag](f: T => Unit): Unit = if (classTag[T].runtimeClass.isInstance(a)) f(a.asInstanceOf[T])

  }

  implicit class GenericExtensions[T](t: T) {
    def use(f: T => Unit): T = {f(t); t}
    def in(seq: Traversable[T]): Boolean = seq.exists(_ == t)
  }

  implicit class TypeTuple(t: (Type, Type)) {

    val c1: Class[_ <: Type] = t._1.getClass
    val c2: Class[_ <: Type] = t._2.getClass

    def anyIs(types: Type*): Boolean = types.map(_.getClass).exists(c => c == c1 || c == c2)
    def bothAre(types: Type*): Boolean = types.map(_.getClass).exists(c => c == c1 && c == c2)
  }

  implicit class TraversableExtensions[Collection[T] <: Traversable[T], T](collection: Collection[T]) {

    def filterType[A <: T : ClassTag]: Collection[A] = collection.filter(classTag[A].runtimeClass.isInstance(_)).asInstanceOf[Collection[A]]
    def filterNotType[A <: T : ClassTag]: Collection[T] = collection.filter(!classTag[A].runtimeClass.isInstance(_)).asInstanceOf[Collection[T]]
    def findInstance[A <: T : ClassTag]: Option[A] = collection.find(classTag[A].runtimeClass.isInstance(_)).asInstanceOf[Option[A]]
    def findDefined[A](f: T => Option[A]): Option[A] = {
      for (v <- collection) {
        val o = f(v)
        if (o.isDefined) return o
      }
      None
    }
  }

  implicit class MutableMapExtensions[K, V](m: mutable.Map[K, V]) {

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
