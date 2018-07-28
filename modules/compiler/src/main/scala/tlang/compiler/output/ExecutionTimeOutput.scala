package tlang.compiler.output
import tlang.compiler.Main
import tlang.formatting.Colors.Color
import tlang.formatting.Formatter
import tlang.utils.Extensions._
import tlang.utils.JSON.Json

case class ExecutionTimeOutput(
  executionTimes: Map[String, Double],
  success: Boolean
)(
  implicit formatter: Formatter
) extends Output {
  override def pretty: String = {

    import formatter._

    def calculateColors(phases: Seq[String]): Map[String, Color] = {
      val colors = Array(Green, Yellow, Red)
      phases
        .sortBy { executionTimes(_) }
        .cut(3)
        .zipWithIndex
        .flatMap { case (names, i) => names.map { _ -> colors(i) } }
        .toMap
    }

    if (executionTimes.isEmpty)
      return ""


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
      .render()
  }



  override def json: Json = Json("executionTimes" -> executionTimes)
}
