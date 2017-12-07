package tlang.utils

import better.files.File

import scala.collection.JavaConverters._
import scala.collection.immutable

object ClassLocator {

  /**
    * Scans all classes accessible from the context class loader which belong to the given package and subpackages.
    *
    * @param packageName The base package
    * @return The classes
    */
  def getClassesInPackage(packageName: String): immutable.IndexedSeq[Class[_]] = {
    val classLoader = Thread.currentThread.getContextClassLoader

    classLoader
      .getResources(packageName.replace('.', '/'))
      .asScala
      .map(File(_))
      .flatMap { dir => dir.glob("**/*.class", includePath = false).flatMap(getClass(packageName, _)) }
      .toIndexedSeq
  }

  private def getClass(packageName: String, file: File): Option[Class[_]] = {
    val dirPath = file.pathAsString.replace(java.io.File.separatorChar, '.')
    val className = dirPath.substring(dirPath.indexOf(packageName)).stripSuffix(".class")
    try {
      Some(Class.forName(className))
    } catch {
      case _: NoClassDefFoundError => None
    }
  }

}
