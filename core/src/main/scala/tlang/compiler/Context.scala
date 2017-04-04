package tlang.compiler

import java.io.File
import java.lang.reflect.Method
import java.net.{URL, URLClassLoader}

import tlang.compiler.error.{Formatting, Reporter, SimpleFormatting}
import tlang.compiler.imports.ClassSymbolLocator

import scala.collection.mutable

case class Context(
  reporter: Reporter,
  files: Set[File] = Set(),
  classPaths: Set[String] = Set(),
  outDirs: Set[File] = Set(new File(".")),
  printCodeStages: Set[String] = Set(),
  formatting: Formatting = SimpleFormatting,
  printInfo: Boolean = false,
  errorContext: Int = 2,
  ignoredImports: Set[String] = Set()
) {

  private val JavaClassPath = "java.class.path"
  val executionTimes: mutable.Map[Pipeline[_, _], Double] = mutable.Map()

  def javaClassPath: Set[String] = System.getProperty(JavaClassPath).split(";").toSet

  def getClassPaths: Set[String] = classPaths ++ javaClassPath + "." + Main.TDirectory

  // Updates the repository in which to search for java classes.
  ClassSymbolLocator.setClassPath(getClassPaths)

  // Ugly hack to inject the class paths in to the current class loader
  private val method: Method = classOf[URLClassLoader].getDeclaredMethod("addURL", classOf[URL])
  method.setAccessible(true)

  for (p <- getClassPaths) {
    val f = new File(p)
    method.invoke(ClassLoader.getSystemClassLoader, f.toURI.toURL)
  }

}
