package tcompiler.utils

import tcompiler.analyzer.Types.Type

/**
  * Created by Tim Lindeberg on 4/16/2016.
  */
object Extensions {

  implicit def ifDefinedOption[T](o: Option[T]) = new {
    def ifDefined(f: T => Unit): Unit = if(o.isDefined) f(o.get)
  }

  implicit class TypeTuple(t: (Type, Type)) {

    val c1 = t._1.getClass
    val c2 = t._2.getClass

    def anyIs[T <: Type](clazzes: Class[T]*) = clazzes.exists(c => c.isAssignableFrom(c1) || c.isAssignableFrom(c2))
    def bothAre[T <: Type](clazzes: Class[T]*) = clazzes.exists(c => c.isAssignableFrom(c1) && c.isAssignableFrom(c2))
  }

  implicit class TraversableExtensions[T](l: Traversable[T]) {

    def filterType[A](clazz: Class[A]) = l.filter(_.getClass == clazz).asInstanceOf[List[A]]

  }


}
