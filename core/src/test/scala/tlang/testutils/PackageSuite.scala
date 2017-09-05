package tlang.testutils

import org.scalatest.Suite
import tlang.utils.ClassLocator

import scala.reflect.{ClassTag, _}


class PackageSuite[T <: Suite : ClassTag](packageName: String) extends Suite {
  thisSuite =>

  /**
    * Returns an immutable <code>IndexedSeq</code> containing the suites passed to the constructor in
    * the order they were passed.
    */
  override lazy val nestedSuites: collection.immutable.IndexedSeq[Suite] = {
    ClassLocator
      .getClassesInPackage(packageName)
      .filter { classTag[T].runtimeClass.isAssignableFrom }
      .map { _.newInstance().asInstanceOf[T] }
  }

  /**
    * Returns a user friendly string for this suite, composed of the
    * simple name of the class (possibly simplified further by removing dollar signs if added by the Scala interpeter) and, if this suite
    * contains nested suites, the result of invoking <code>toString</code> on each
    * of the nested suites, separated by commas and surrounded by parentheses.
    *
    * @return a user-friendly string for this suite
    */
  override def toString: String = packageName
}
