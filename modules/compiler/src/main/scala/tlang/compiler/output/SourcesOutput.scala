package tlang.compiler.output
import tlang.formatting.Formatter
import tlang.formatting.grid.{CenteredColumn, EvenlySpaced}
import tlang.utils.JSON.Json
import tlang.utils.Source

case class SourcesOutput(sources: List[Source]) extends Output {
  override def pretty(formatter: Formatter): String = {
    val formatting = formatter.formatting
    import formatting._

    val numSources = sources.size
    val end = if (numSources > 1) "sources" else "source"

    val grid = formatter.grid.header(Bold("Compiling") + " " + Blue(numSources) + " " + Bold(end))

    val descriptions = sources.map(_.description(formatting)).sorted
    grid
      .row(CenteredColumn)
      .content(EvenlySpaced(descriptions))
      .render()
  }

  override def json: Json = Json("sources" -> sources.map(_.description))
}
