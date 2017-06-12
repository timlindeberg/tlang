package tlang.compiler

import cafebabe.StackTrace
import tlang.compiler.ast.Trees.CompilationUnit
import tlang.compiler.lexer.Token
import tlang.utils.Extensions._
import tlang.utils.formatting.Formatting

case class DebugOutputFormatter(phaseName: String, formatting: Formatting) {

  import formatting._

  private val tabWidth = 2

  def formatStackTraces(stackTraces: List[StackTrace]): String = {
    val legend = s"$Bold%-6s %-6s %-7s %-15s%s$Reset"
      .format("Line", "PC", "Height", "ByteCode", "Info")

    val blocks = stackTraces.flatMap { st =>
      val header = center(st.header) + "\n\n" + legend
      header :: st.content :: Nil
    }
    format(blocks)
  }

  def formatTokens(allTokens: List[List[Token]]): String = {
    import formatting._

    val legend = s"$Bold%-35s %-16s %s$Reset".format("Text", "Token", "Position")
    val blocks = allTokens.flatMap { tokens =>
      val source = tokens.head.source
      val header = center(formatFileName(source.mainName)) + "\n\n" + legend
      val body = tokens.map(formatToken(_, formatting)).mkString("\n")
      header :: body :: Nil
    }
    format(blocks)
  }

  def formatASTs(cus: List[CompilationUnit]): String = {
    val mediumHeaderColor = Blue + Bold
    val blocks = cus.flatMap { cu =>
      val printedTree = treePrinter(cu).trimWhiteSpaces
      center(formatFileName(cu.source.mainName)) ::
      center(mediumHeaderColor("Pretty printed code")) + "\n\n" +
      prettyPrinter(cu).replaceAll("\t", " " * tabWidth).trimWhiteSpaces ::
      center(mediumHeaderColor("Formatted AST")) + "\n\n" + treePrinter.header ::
      printedTree ::
      Nil
    }
    format(blocks)
  }

  private def format(blocks: List[String]) = makeBox(header, blocks)
  private def header = Bold("Output after ") + Blue(phaseName.capitalize)

  private def formatToken(token: Token, formatting: Formatting) = {
    import formatting._

    val tokenName = token.kind.getClass.getSimpleName.dropRight(1).replaceAll("KIND", "")
    val text = token.toString
    val trimmed = if (text.charCount >= 35) text.takeChars(31) + "..." else text
    val start = NumColor(token.line) + ":" + NumColor(token.col)
    val end = NumColor(token.endLine) + ":" + NumColor(token.endCol)
    val pos = s"$start - $end"
    s"$Blue%-35s$Reset $Bold%-16s$Reset %s".format(trimmed, tokenName, pos)
  }
}