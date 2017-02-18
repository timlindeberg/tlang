package tlang.compiler

import java.io.File

import tlang.compiler.ast.PrettyPrinter
import tlang.compiler.error.{Formatting, Reporter, SimpleFormatting}
import tlang.compiler.imports.ClassSymbolLocator
import tlang.compiler.main.Main
import tlang.utils.Colors

import scala.collection.mutable

case class Context(
  reporter: Reporter,
  files: Set[File],
  classPaths: Set[String] = Set(),
  outDirs: Set[File] = Set(new File(".")),
  printCodeStages: Set[String] = Set(),
  formatting: Formatting = SimpleFormatting,
  printer: PrettyPrinter = PrettyPrinter(Colors(isActive = false)),
  printInfo: Boolean = false,
  ignoredImports: Set[String] = Set()
) {

  private val JavaClassPath = "java.class.path"
  val executionTimes: mutable.Map[Pipeline[_, _], Double] = mutable.Map()

  def javaClassPath: Set[String] = System.getProperty(JavaClassPath).split(";").toSet

  def getClassPaths: Set[String] = classPaths ++ javaClassPath + "." + Main.TDirectory

  // Updates the repository in which to search for java classes.
  ClassSymbolLocator.setClassPath(getClassPaths)

}
