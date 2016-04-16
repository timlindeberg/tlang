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

    def anyIs(types: Type*) = types.map(_.getClass).exists(c => c == c1 || c == c2)
    def bothAre(types: Type*) = types.map(_.getClass).exists(c => c == c1 && c == c2)
  }


}
