package tcompiler.utils

import tcompiler.analyzer.Types.Type

/**
  * Created by Tim Lindeberg on 4/16/2016.
  */
object Extensions {

  implicit def ifDefinedOption[T](o: Option[T]) = new {
    def ifDefined(f: T => Unit): Unit = if(o.isDefined) f(o.get)
  }

  implicit def ifInstanceOf(any: Any) = new {
    def ifInstanceOf[T](f: T => Unit) = if(any.isInstanceOf[T]) f(any.asInstanceOf[T])
  }

  implicit class TypeTuple(t: (Type, Type)) {

    val c1 = t._1.getClass
    val c2 = t._2.getClass

    def anyIs(types: Type*) = types.map(_.getClass).exists(c => c == c1 || c == c2)
    def bothAre(types: Type*) = types.map(_.getClass).exists(c => c == c1 && c == c2)
  }

  implicit class ListExt[T](l: List[T]) {

    def filterType[A](clazz: Class[A]) = l.filter(_.getClass == clazz).asInstanceOf[List[A]]

  }


}
