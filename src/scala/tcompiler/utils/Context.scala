package tcompiler
package utils

import java.io.File
import java.net.{URL, URLClassLoader}

import tcompiler.imports.ClassSymbolLocator

case class Context(
  reporter: Reporter,
  files: List[File],
  classPaths: List[String] = Nil,
  outDir: Option[File] = None
) {

  private val JavaClassPath = "java.class.path"

  val defaultClassPaths = List(Main.TDirectory, ".")

  def getClassPaths =
    classPaths :::
      defaultClassPaths :::
      System.getProperty(JavaClassPath).split(";").toList

  // Updates the repository in which to search for java classes.
  ClassSymbolLocator.setClassPath(getClassPaths)

  val method = classOf[URLClassLoader].getDeclaredMethod("addURL", classOf[URL])
  method.setAccessible(true)

  for(p <- classPaths ::: defaultClassPaths) {
    val f = new File(p)
    method.invoke(ClassLoader.getSystemClassLoader, f.toURI.toURL)
  }


}
