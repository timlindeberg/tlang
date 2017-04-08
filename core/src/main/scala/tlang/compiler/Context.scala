package tlang.compiler

import java.io.File

import tlang.compiler.error.Reporter
import tlang.utils.formatting.{Formatting, SimpleFormatting}

import scala.collection.mutable

case class Context(
  reporter: Reporter,
  files: Set[File] = Set(),
  private val classPaths: Set[String] = Set(),
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

}
