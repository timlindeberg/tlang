package tlang.compiler.output
import tlang.formatting.Formatter
import tlang.utils.Extensions._
import tlang.utils.JSON.Json

case class InterruptedOutput() extends Output {
  override def pretty(formatter: Formatter): String = {
    import formatter.formatting._
    val color = Bold + Red
    NL +  formatter.grid.header(color("Compilation interrupted.")).render()
  }
  override def json: Json = Json("interrupted" -> true)
}
