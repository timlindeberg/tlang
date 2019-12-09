package tlang
package compiler
package imports

import java.io.File
import java.nio.file.{Files, Paths}

import scala.collection.JavaConverters._
import scala.collection.mutable.ListBuffer

object ClassPath {

  lazy val Default: ClassPath = {
    val javaLibDirectory = Paths.get(System.getProperty("java.home") + File.separator + "lib")

    val javaLibJarFiles: Set[String] =
      Files
        .find(javaLibDirectory, 1000, (path, attr) =>
          attr.isRegularFile && path.getFileName.toString.matches(".*\\.jar")
        )
        .iterator
        .asScala
        .map(_.toString)
        .toSet

    val javaClassPath = System.getProperty("java.class.path").split(File.pathSeparator).toSet
    ClassPath(javaClassPath ++ javaLibJarFiles + Constants.TStdLibDirectory)
  }
  val Empty = new ClassPath(Map(), Array(), Set())

  def apply(): ClassPath = Empty
  def apply(paths: Set[String]): ClassPath = {
    val classPathParser = new ClassPathParser(paths)
    val (pathToFile, classes) = classPathParser.parse()
    new ClassPath(pathToFile, classes, paths)
  }
}

// This should be a regular class since we want equals to use reference comparison.
// Making this a case class will use Scalas default hashCode implementation which will be
// very slow for this class.
class ClassPath private(val pathToFile: Map[String, ClassFile], val classes: Array[String], val paths: Set[String]) {

  def +(path: String): ClassPath = this ++ ClassPath(Set(path))

  def ++(paths: Set[String]): ClassPath = this ++ ClassPath(paths)

  def ++(other: ClassPath): ClassPath = {
    if (this.isEmpty) return other
    if (other.isEmpty) return this

    val allClasses = classes ++ other.classes
    // Use insertion sort since it's very fast for almost sorted arrays
    insertionSort(allClasses)
    new ClassPath(pathToFile ++ other.pathToFile, allClasses, paths ++ other.paths)
  }

  def apply(className: String): Option[ClassFile] = pathToFile.get(ImportUtils.toPath(className))

  def getClassesInPackage(packageName: String): List[String] = {
    val name = ImportUtils.toPath(packageName)
    var index = getStartPosition(name)
    if (index == -1)
      return Nil

    val packageLevel = name.count(_ == '/')

    val listBuff = ListBuffer[String]()
    while (index < classes.length && classes(index).startsWith(name)) {
      val clazz = classes(index)

      // Check if the class belongs to the same package or if it's part of a subpackage
      val clazzPackageLevel = clazz.count(_ == '/') - 1
      if (clazzPackageLevel == packageLevel)
        listBuff += ImportUtils.toTName(clazz)
      index += 1
    }
    listBuff.toList
  }

  def size: Int = paths.size
  def isEmpty: Boolean = size == 0
  def nonEmpty: Boolean = size != 0

  def exists(className: String): Boolean = classes.binarySearch(className) != -1

  private def getStartPosition(path: String): Int = {
    if (classes.isEmpty)
      return -1

    var low = 0
    var high = classes.length - 1

    while (low <= high) {
      val mid = (low + high) >>> 1
      val midVal = classes(mid)

      if (midVal < path)
        low = mid + 1
      else if (midVal > path)
        high = mid - 1
      else if (low != mid) // Equal but range is not fully scanned
        high = mid // Set upper bound to current number and rescan
    }

    // low will point to first index that starts with 'path' if such an index exists in the list
    if (classes(low).startsWith(path)) low else -1
  }

  private def insertionSort(array: Array[String]): Unit = {
    var i = 0
    while (i < array.length) {
      var j = i
      while (j > 0 && array(j) < array(j - 1)) {
        val tmp = array(j)
        array(j) = array(j - 1)
        array(j - 1) = tmp

        j -= 1
      }
      i += 1
    }
  }
}
