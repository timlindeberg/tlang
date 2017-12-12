package tlang.compiler.utils

import cafebabe.CodegenerationStackTrace
import tlang.compiler.ast.Trees.Tree
import tlang.compiler.ast.{PrettyPrinter, TreePrinter}
import tlang.compiler.lexer.Token
import tlang.formatting.Formatter
import tlang.formatting.grid.Alignment.Center
import tlang.formatting.grid.{Column, TruncatedColumn}
import tlang.utils.Extensions._

object DebugOutputFormatter {

  def apply(formatter: Formatter): DebugOutputFormatter = apply(
    formatter,
    TreePrinter(formatter),
    PrettyPrinter(formatter.formatting)
  )

}
case class DebugOutputFormatter(formatter: Formatter, treePrinter: TreePrinter, prettyPrinter: PrettyPrinter) {

  import formatter.formatting._

  private val TabWidth = 2

  def printStackTraces(phaseName: String, stackTraces: List[CodegenerationStackTrace]): Unit = {
    val grid = makeGrid(phaseName)
    stackTraces.foreach { stackTrace =>
      grid
        .row(alignment = Center)
        .content(stackTrace.header)
        .row(5)
        .columnHeaders("Line", "PC", "Height", "ByteCode", "Info")
        .contents(stackTrace.content)
    }
    grid.print()
  }

  def printTokens(phaseName: String, allTokens: List[List[Token]]): Unit = {
    val grid = makeGrid(phaseName)

    allTokens.foreach { tokens =>
      grid
        .row(alignment = Center)
        .content(formatter.fileName(tokens.head.sourceName))
        .row(TruncatedColumn, Column, Column, Column)
        .columnHeaders("Text", "Token", "Start", "End")
        .mapContent(tokens) { token =>
          val tokenName = token.kind.getClass.getSimpleName.dropRight(1).replaceAll("KIND", "")
          val start = NumColor(token.line) + ":" + NumColor(token.col)
          val end = NumColor(token.lineEnd) + ":" + NumColor(token.colEnd)
          (token.toString.replaceAll(NL, ""), Bold(tokenName), start, end)
        }
    }
    grid.print()
  }

  def printASTs(phaseName: String, trees: List[Tree]): Unit = {
    val grid = makeGrid(phaseName)
    trees.foreach { tree =>
      grid
        .row(alignment = Center)
        .content(formatter.fileName(tree.sourceName))
        .row()
        .content(prettyPrinter(tree).replaceAll("\t", " " * TabWidth).trimWhiteSpaces)
        .row(TruncatedColumn, Column, Column, Column)
        .columnHeaders("Tree", "Reference", "Symbol", "Type")
        .contents(treePrinter(tree))
    }
    grid.print()
  }

  private def makeGrid(phaseName: String) =
    formatter.grid.header(Bold("Output after ") + Blue(phaseName.capitalize))
}