package tlang
package compiler

import better.files.File
import tlang.compiler.argument.{DirectoryFlag, IgnoredDefaultImportsFlag, PrintOutputFlag, ThreadsFlag}
import tlang.compiler.imports.ClassPath
import tlang.compiler.messages.Reporter
import tlang.compiler.output.OutputHandler
import tlang.formatting.Formatter
import tlang.options.Options
import tlang.utils.{Executor, SingleThreadExecutor}

import scala.collection.mutable

object Context {
  def apply(
    reporter: Reporter,
    outputHandler: OutputHandler,
    classPath: ClassPath,
    options: Options
  )(
    implicit formatter: Formatter
  ): Context = {
    Context(
      reporter,
      outputHandler,
      classPath,
      options,
      executor = options(ThreadsFlag),
      outDirs = options(DirectoryFlag),
      printCodePhase = options(PrintOutputFlag),
      ignoredImports = options(IgnoredDefaultImportsFlag)
    )
  }
}

case class Context(
  reporter: Reporter,
  output: OutputHandler,
  classPath: ClassPath = ClassPath.Empty,
  options: Options = Options.Empty,
  executor: Executor = SingleThreadExecutor,
  outDirs: Set[File] = Set(File(".")),
  printCodePhase: Set[String] = Set(),
  ignoredImports: Set[String] = Set()
) (
  implicit val formatter: Formatter,
){

  val executionTimes: mutable.Map[String, Double] = mutable.Map()
  def allClassPaths: Set[String] = outDirs.map(_.pathAsString) ++ classPath.paths
}
