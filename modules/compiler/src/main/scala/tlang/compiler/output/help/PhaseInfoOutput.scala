package tlang.compiler.output.help

import tlang.compiler.CompilerPhase
import tlang.compiler.output.Output
import tlang.formatting.Formatter
import tlang.utils.JSON.Json

case class PhaseInfoOutput(phases: Seq[CompilerPhase[_, _]] ) extends Output {
  override def pretty(formatter: Formatter): String = {
    val formatting = formatter.formatting
    import formatting._

    formatter
      .grid
      .header(Bold(s"Phases of the T-Compiler"))
      .row(2)
      .mapContent(phases) { phase => (Magenta(phase.phaseName.capitalize), phase.description(formatting)) }
      .render()
  }

  override def json: Json = Json("phases" -> phases.map(_.json))
}
