package tlang


import better.files.File
import tlang.compiler.imports.ClassPath
import tlang.compiler.{CompilerPhase, DebugOutputFormatter}
import tlang.formatting.{Formatter, Formatting}
import tlang.messages.Reporter

import scala.collection.mutable

case class Context(
  reporter: Reporter,
  formatter: Formatter,
  debugOutputFormatter: DebugOutputFormatter,
  files: Set[File] = Set(),
  classPath: ClassPath = ClassPath.Empty,
  outDirs: Set[File] = Set(File(".")),
  printCodePhase: Set[String] = Set(),
  ignoredImports: Set[String] = Set()
) {

  val executionTimes: mutable.Map[CompilerPhase[_, _], Double] = mutable.Map()

  def formatting: Formatting = formatter.formatting

}
