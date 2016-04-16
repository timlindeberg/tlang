package tcompiler.utils

/**
  * Created by Tim Lindeberg on 4/16/2016.
  */
object Extensions {

  implicit def ifDefinedOption[T](o: Option[T]) = new {
    def ifDefined(f: T => Unit): Unit = if(o.isDefined) f(o.get)
  }
}
