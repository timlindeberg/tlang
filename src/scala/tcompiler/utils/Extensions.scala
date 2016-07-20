package tcompiler.utils

import tcompiler.analyzer.Types.Type
import scala.collection.mutable

/**
  * Created by Tim Lindeberg on 4/16/2016.
  */
object Extensions {

  implicit class OptionExtensions[T](o: Option[T]) {

    def ifDefined(f: T => Unit): Unit = if (o.isDefined) f(o.get)

  }

  implicit class TypeTuple(t: (Type, Type)) {

    val c1 = t._1.getClass
    val c2 = t._2.getClass

    def anyIs(types: Type*) = types.map(_.getClass).exists(c => c == c1 || c == c2)
    def bothAre(types: Type*) = types.map(_.getClass).exists(c => c == c1 && c == c2)
  }

  implicit class TraversableExtensions[T](l: Traversable[T]) {

    def filterType[A](clazz: Class[A]) = l.filter(_.getClass == clazz).asInstanceOf[List[A]]

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
