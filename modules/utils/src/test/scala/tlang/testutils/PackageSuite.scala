package tlang
package testutils

import org.scalatest.Suite
import tlang.utils.ClassLocator

import scala.reflect.{ClassTag, _}


class PackageSuite[T <: Suite : ClassTag](packageName: String) extends Suite {

  override lazy val nestedSuites: collection.immutable.IndexedSeq[Suite] = {
    ClassLocator
      .getClassesInPackage(packageName)
      .filter { classTag[T].runtimeClass.isAssignableFrom }
      .map { _.newInstance().asInstanceOf[T] }
  }


  override val suiteName: String = packageName.split("\\.").last.capitalize
}
