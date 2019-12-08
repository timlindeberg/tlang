package tlang
package formatting
package grid


import scala.collection.mutable.ListBuffer

object ColumnDefaults {
  val Width: tlang.formatting.grid.Width = tlang.formatting.grid.Width.Auto
  val OverflowHandling: tlang.formatting.grid.OverflowHandling = tlang.formatting.grid.OverflowHandling.Wrap
  val Alignment: tlang.formatting.grid.Alignment = tlang.formatting.grid.Alignment.Left
}

object Column extends Column(ColumnDefaults.Width, ColumnDefaults.Alignment, ColumnDefaults.OverflowHandling)
object CenteredColumn extends Column(ColumnDefaults.Width, Alignment.Center, ColumnDefaults.OverflowHandling)
object TruncatedColumn extends Column(ColumnDefaults.Width, ColumnDefaults.Alignment, OverflowHandling.Truncate)

case class Column(
  width: Width = ColumnDefaults.Width,
  alignment: Alignment = ColumnDefaults.Alignment,
  overflowHandling: OverflowHandling = ColumnDefaults.OverflowHandling
) {

  private[grid] val lines: ListBuffer[GridContent] = ListBuffer()
  private var _maxWidth = 0

  def maxWidth: Int = _maxWidth

  def addLine(content: GridContent): Unit = {
    content.width ifDefined { width => _maxWidth = Math.max(_maxWidth, width) }
    lines += content
  }

  def content: String = lines.map(content => content.render(_maxWidth)).mkString("\n")
}
