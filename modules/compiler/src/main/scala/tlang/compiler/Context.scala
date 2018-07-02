package tlang.compiler

import better.files.File
import tlang.compiler.argument.{DirectoryFlag, IgnoredDefaultImportsFlag, PrintOutputFlag, ThreadsFlag}
import tlang.compiler.imports.ClassPath
import tlang.compiler.messages.Reporter
import tlang.compiler.output.OutputHandler
import tlang.formatting.{Formatter, Formatting}
import tlang.options.Options
import tlang.utils.{Executor, SingleThreadExecutor}

import scala.collection.mutable

object Context {
  def apply(
    reporter: Reporter,
    formatter: Formatter,
    outputHandler: OutputHandler,
    classPath: ClassPath,
    options: Options
  ): Context = {
    Context(
      reporter,
      formatter,
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
  formatter: Formatter,
  output: OutputHandler,
  classPath: ClassPath = ClassPath.Empty,
  options: Options = Options.Empty,
  executor: Executor = SingleThreadExecutor,
  outDirs: Set[File] = Set(File(".")),
  printCodePhase: Set[String] = Set(),
  ignoredImports: Set[String] = Set()
) {

  val executionTimes: mutable.Map[String, Double] = mutable.Map()

  def formatting: Formatting = formatter.formatting

  def allClassPaths: Set[String] = outDirs.map(_.pathAsString) ++ classPath.paths
}
