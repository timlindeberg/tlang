package tcompiler.utils

import tcompiler.analyzer.Types.Type

import scala.collection.mutable
import scala.reflect.ClassTag
import scala.reflect.runtime._
import scala.reflect.runtime.universe._
import scala.reflect._
/**
  * Created by Tim Lindeberg on 4/16/2016.
  */
object Extensions {

  implicit class OptionExtensions[T](o: Option[T]) {

    def ifDefined(f: T => Unit): Unit = if (o.isDefined) f(o.get)

  }

  implicit class AnyExtensions(a: Any) {

    def ifInstanceOf[T: ClassTag](f: T => Unit) = if(classTag[T].runtimeClass.isInstance(a)) f(a.asInstanceOf[T])

  }

  implicit class TypeTuple(t: (Type, Type)) {

    val c1 = t._1.getClass
    val c2 = t._2.getClass

    def anyIs(types: Type*) = types.map(_.getClass).exists(c => c == c1 || c == c2)
    def bothAre(types: Type*) = types.map(_.getClass).exists(c => c == c1 && c == c2)
  }

  implicit class TraversableExtensions[Collection[T] <: Traversable[T], T](l: Collection[T]) {

    def filterType[A <: T: ClassTag]: Collection[A] = l.filter(classTag[A].runtimeClass.isInstance(_)).asInstanceOf[Collection[A]]
    def filterNotType[A <: T: ClassTag]: Collection[T] = l.filter(!classTag[A].runtimeClass.isInstance(_)).asInstanceOf[Collection[T]]
    def findInstance[A <: T: ClassTag]: A = l.find(classTag[A].runtimeClass.isInstance(_)).get.asInstanceOf[A]

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
