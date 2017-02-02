package tcompiler
package utils

import java.io.File
import java.lang.reflect.Method
import java.net.{URL, URLClassLoader}

import tcompiler.ast.PrettyPrinter
import tcompiler.error.{Formatting, Reporter, SimpleFormatting}
import tcompiler.imports.ClassSymbolLocator

import scala.collection.mutable

case class Context(
  reporter: Reporter,
  files: List[File],
  classPaths: List[String] = Nil,
  outDirs: List[File] = List(new File(".")),
  printCodeStages: Set[String] = Set(),
  formatting: Formatting = SimpleFormatting,
  printer: PrettyPrinter = new PrettyPrinter(Colorizer(false)),
  printInfo: Boolean = false,
  ignoredImports: Set[String] = Set()
) {

  private val JavaClassPath = "java.class.path"
  val executionTimes: mutable.Map[Pipeline[_, _], Double] = mutable.Map()

  def getClassPaths: List[String] = "." :: classPaths ::: System.getProperty(JavaClassPath).split(";").toList

  // Updates the repository in which to search for java classes.
  ClassSymbolLocator.setClassPath(getClassPaths)

  val method: Method = classOf[URLClassLoader].getDeclaredMethod("addURL", classOf[URL])
  method.setAccessible(true)

  for (p <- getClassPaths) {
    val f = new File(p)
    method.invoke(ClassLoader.getSystemClassLoader, f.toURI.toURL)
  }


}
