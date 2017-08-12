package tlang.compiler

import cafebabe.StackTrace
import tlang.compiler.ast.Trees.CompilationUnit
import tlang.compiler.lexer.Token
import tlang.utils.Extensions._
import tlang.utils.formatting.Formatting
import tlang.utils.formatting.grid.Alignment.Center
import tlang.utils.formatting.grid.OverflowHandling.Truncate
import tlang.utils.formatting.grid.{Column, Grid}

case class DebugOutputFormatter(phaseName: String, formatting: Formatting) {

  import formatting._

  private val TabWidth    = 2
  private val HeaderColor = Bold + Blue

  def printStackTraces(stackTraces: List[StackTrace]): Unit = {
    val grid = makeGrid()
    stackTraces.foreach { stackTrace =>
      grid
        .row(alignment = Center)
        .content(stackTrace.header)
        .row(5)
        .content(HeaderColor("Line"), HeaderColor("PC"), HeaderColor("Height"), HeaderColor("ByteCode"), HeaderColor("Info"))
        .content()
        .contents(stackTrace.content)
    }
    grid.print()
  }

  def printTokens(allTokens: List[List[Token]]): Unit = {
    import formatting._

    val grid = makeGrid()

    allTokens.foreach { tokens =>
      val source = tokens.head.source
      grid
        .row(alignment = Center)
        .content(formatFileName(source.mainName))
        .row(Column(overflowHandling = Truncate), Column, Column)
        .content(HeaderColor("Text"), HeaderColor("Token"), HeaderColor("Position"))
        .content()
        .mapContent(tokens) { token =>
          val tokenName = token.kind.getClass.getSimpleName.dropRight(1).replaceAll("KIND", "")
          val start = NumColor(token.line) + ":" + NumColor(token.col)
          val end = NumColor(token.endLine) + ":" + NumColor(token.endCol)
          (token.toString, Bold(tokenName), s"$start - $end")
        }
    }
    grid.print()
  }

  def printASTs(cus: List[CompilationUnit]): Unit = {
    val grid = makeGrid()
    cus.foreach { cu =>
      grid
        .row(alignment = Center)
        .content(formatFileName(cu.source.mainName))
        .row()
        .content(prettyPrinter(cu).replaceAll("\t", " " * TabWidth).trimWhiteSpaces)
        .row(Column(overflowHandling = Truncate), Column, Column)
        .content(HeaderColor("Tree"), HeaderColor("Symbol"), HeaderColor("Type"))
        .content()
        .contents(treePrinter(cu))
    }
    grid.print()
  }

  private def makeGrid() = Grid(formatting).header(Bold("Output after ") + Blue(phaseName.capitalize))
}
