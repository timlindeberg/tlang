package tlang

import java.io.File

import tlang.compiler.error.Reporter
import tlang.compiler.imports.ClassPath
import tlang.compiler.{CompilerPhase, DebugOutputFormatter}
import tlang.formatting.{Formatter, Formatting}

import scala.collection.mutable

case class Context(
  reporter: Reporter,
  formatter: Formatter,
  debugOutputFormatter: DebugOutputFormatter,
  files: Set[File] = Set(),
  classPath: ClassPath = ClassPath(),
  outDirs: Set[File] = Set(new File(".")),
  printCodePhase: Set[String] = Set(),
  ignoredImports: Set[String] = Set()
) {

  val executionTimes: mutable.Map[CompilerPhase[_, _], Double] = mutable.Map()

  def formatting: Formatting = formatter.formatting

}
