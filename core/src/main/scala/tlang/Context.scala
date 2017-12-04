package tlang


import better.files.File
import tlang.compiler.DebugOutputFormatter
import tlang.compiler.Main.CompilerPhases
import tlang.compiler.imports.ClassPath
import tlang.formatting.Colors.Color
import tlang.formatting.{Formatter, Formatting}
import tlang.messages.Reporter
import tlang.utils.Extensions._

import scala.collection.mutable

case class Context(
  reporter: Reporter,
  formatter: Formatter,
  debugOutputFormatter: DebugOutputFormatter,
  classPath: ClassPath = ClassPath.Empty,
  outDirs: Set[File] = Set(File(".")),
  printCodePhase: Set[String] = Set(),
  ignoredImports: Set[String] = Set()
) {

  val executionTimes: mutable.Map[String, Double] = mutable.Map()

  def formatting: Formatting = formatter.formatting

  def printExecutionTimes(): Unit = {
    if (executionTimes.isEmpty)
      return

    val formatting = formatter.formatting
    import formatting._

    val totalTime = executionTimes.values.sum
    val executedPhases = CompilerPhases
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

    formatter
      .grid
      .header(f"${ Bold }Compilation executed ${ Green("successfully") }$Bold in $Green$totalTime%.2f$Reset ${ Bold }seconds.$Reset")
      .row(3)
      .content(Blue("Phase"), Blue("Time (s)"), Blue("Percentage"))
      .content()
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
