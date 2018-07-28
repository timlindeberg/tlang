package tlang.compiler.output.help

import tlang.compiler.CompilerPhase
import tlang.compiler.output.Output
import tlang.formatting.Formatter
import tlang.utils.JSON.Json

case class PhaseInfoOutput(phases: Seq[CompilerPhase[_, _]])(implicit formatter: Formatter) extends Output {

  override def pretty: String = {
    import formatter.formatting._

    formatter
      .grid
      .header(Bold(s"Phases of the T-Compiler"))
      .row(2)
      .mapContent(phases) { phase => (Magenta(phase.phaseName.capitalize), phase.description) }
      .render()
  }

  override def json: Json = Json("phases" -> phases.map(_.json))
}
