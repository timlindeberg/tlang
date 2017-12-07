package tlang.compiler

import better.files.File
import tlang.compiler.imports.ClassPath
import tlang.compiler.messages.Reporter
import tlang.compiler.utils.DebugOutputFormatter
import tlang.formatting.Colors.Color
import tlang.formatting.{Formatter, Formatting}
import tlang.utils.Extensions._
import tlang.utils.{Executor, SingleThreadExecutor}

import scala.collection.mutable

case class Context(
  reporter: Reporter,
  formatter: Formatter,
  debugOutputFormatter: DebugOutputFormatter,
  classPath: ClassPath = ClassPath.Empty,
  executor: Executor = SingleThreadExecutor,
  outDirs: Set[File] = Set(File(".")),
  printCodePhase: Set[String] = Set(),
  ignoredImports: Set[String] = Set()
) {

  val executionTimes: mutable.Map[String, Double] = mutable.Map()

  def formatting: Formatting = formatter.formatting

  def allClassPaths: Set[String] = outDirs.map(_.pathAsString) ++ classPath.paths

  def printExecutionTimes(success: Boolean): Unit = {
    if (executionTimes.isEmpty)
      return

    val formatting = formatter.formatting
    import formatting._

    val totalTime = executionTimes.values.sum
    val executedPhases = Main.CompilerPhases
      .map(_.phaseName)
      .filter(executionTimes.contains)

    val colors = calculateColors(executedPhases)
    val columns = executedPhases
      .map { phase =>
        val time = executionTimes(phase)
        val percentage = ((time / totalTime) * 100).asInstanceOf[Int]
        val color = colors(phase)
        (Blue(phase.capitalize), color(f"$time%.2f"), color(f"$percentage%02d"))
      }

    val (successOrFailure, color) = if (success) ("succeeded", Green) else ("failed", Red)
    val header =
      f"${ Bold }Compilation ${ color(successOrFailure) }$Bold after $color$totalTime%.2f$Reset ${ Bold }seconds.$Reset"
    formatter
      .grid
      .header(header)
      .row(3)
      .columnHeaders("Phase", "Time (s)", "Percentage")
      .contents(columns)
      .print()
  }

  private def calculateColors(phases: Seq[String]): Map[String, Color] = {
    val formatting = formatter.formatting
    import formatting._

    val colors = Array(Green, Yellow, Red)
    phases
      .sortBy { executionTimes(_) }
      .cut(3)
      .zipWithIndex
      .flatMap { case (names, i) => names.map { _ -> colors(i) } }
      .toMap
  }


}
