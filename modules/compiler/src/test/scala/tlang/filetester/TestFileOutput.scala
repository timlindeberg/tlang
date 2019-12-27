package tlang
package filetester

import tlang.compiler.output.Output
import tlang.formatting.Formatter
import tlang.formatting.grid.CenteredColumn
import tlang.utils.FileSource
import tlang.utils.JSON.Json

case class TestFileOutput(source: FileSource, testResult: TestResult)(implicit formatter: Formatter) extends Output {
  override def pretty: String = {
    import formatter._

    val color = if (testResult.success) Green else Red
    val endOfMessage = if (testResult.success) "was successful." else "failed."
    val header = s"${ color("Test of file") } ${ source.getDescription(color + Bold) } ${ color(endOfMessage) }"

    if (testResult.success) {
      return grid
        .header(header)
        .render()
    }

    val box = grid
      .header(header)
      .row()
      .content(testResult.reason)
      .render()

    val boxes = box :: testResult.extraInfo
    boxes.mkString(NL)
  }

  override def json: Json = {
    val reason = if (testResult.success) None else Some(testResult.reason)
    Json(
      "success" -> testResult.success,
      "reason" -> reason
    )
  }
}
