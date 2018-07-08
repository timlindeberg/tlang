package tlang.compiler.output.debug

import tlang.compiler.ast.Trees.Tree
import tlang.compiler.ast.{PrettyPrinter, TreePrinter}
import tlang.compiler.output.Output
import tlang.formatting.Formatter
import tlang.formatting.grid.Alignment.Center
import tlang.formatting.grid.{Column, TruncatedColumn}
import tlang.utils.Extensions._
import tlang.utils.JSON.Json

object ASTOutput {

  def apply(formatter: Formatter, phaseName: String, trees: List[Tree]): ASTOutput = {
    val prettyPrinter = PrettyPrinter(formatter.formatting)
    val treePrinter = TreePrinter(formatter)
    ASTOutput(formatter, prettyPrinter, treePrinter, phaseName, trees)
  }
}

case class ASTOutput(
  formatter: Formatter,
  prettyPrinter: PrettyPrinter,
  treePrinter: TreePrinter,
  phaseName: String,
  trees: List[Tree]
) extends Output {

  private val TabWidth = 2

  override def pretty: String = {
    val formatting = formatter.formatting
    import formatting._

    val grid = formatter.grid.header(Bold("Output after ") + Blue(phaseName.capitalize))
    trees foreach { tree =>
      grid
        .row(alignment = Center)
        .content(tree.sourceDescription(formatting))
        .row()
        .content(prettyPrinter(tree).replaceAll("\t", " " * TabWidth).trimWhiteSpaces)
        .row(Column, TruncatedColumn, Column, Column, TruncatedColumn)
        .columnHeaders("Line", "Tree", "Reference", "Symbol", "Type")
        .contents(treePrinter(tree))
    }
    grid.render()
  }
  override def json: Json = Json()
}
