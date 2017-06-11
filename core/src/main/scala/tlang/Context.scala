package tlang

import java.io.File

import tlang.compiler.CompilerPhase
import tlang.compiler.error.Reporter
import tlang.compiler.imports.ClassPath
import tlang.utils.formatting.{Formatting, SimpleFormatting}

import scala.collection.mutable

case class Context(
  reporter: Reporter,
  files: Set[File] = Set(),
  classPath: ClassPath = ClassPath(),
  outDirs: Set[File] = Set(new File(".")),
  printCodePhase: Set[String] = Set(),
  formatting: Formatting = SimpleFormatting,
  printInfo: Boolean = false,
  errorContext: Int = 2,
  ignoredImports: Set[String] = Set()
) {

  val executionTimes: mutable.Map[CompilerPhase[_, _], Double] = mutable.Map()

}
