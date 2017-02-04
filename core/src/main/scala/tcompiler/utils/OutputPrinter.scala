package tcompiler.utils

import cafebabe.StackTrace
import tcompiler.ast.Trees._
import tcompiler.lexer.Token
import tcompiler.utils.Extensions._

/**
  * Created by Tim Lindeberg on 2/1/2017.
  */

case class OutputPrinter[-F, +T](stage: Pipeline[F, T], ctx: Context) {

  import ctx.formatting._
  import ctx.formatting.colors._

  private val stageName   = stage.stageName.capitalize
  private val shouldPrint = ctx.printCodeStages.contains(stageName.toLowerCase)
  private val header      = Bold("Output after ") + Blue(stageName)


  def printCode[S >: T](output: S): Unit = {
    if (!shouldPrint)
      return

    output match {
      case (_: CompilationUnit) :: _ => printCompilationUnits(output.asInstanceOf[List[CompilationUnit]])
      case (_: StackTrace) :: _      => printStackTraces(output.asInstanceOf[List[StackTrace]])
      case ((_: Token) :: _) :: _    => printTokens(output.asInstanceOf[List[List[Token]]])
      case _                         =>
    }
  }

  private def printCompilationUnits(cus: List[CompilationUnit]) = {
    val blocks = cus.flatMap { cu => center(formatFileName(cu.file)) :: ctx.printer(cu).trimWhiteSpaces :: Nil }
    print(makeBox(header, blocks))
  }

  private def printStackTraces(stackTraces: List[StackTrace]) = {
    val legend = s"$Bold%-6s %-6s %-7s %-15s%s$Reset"
      .format("Line", "PC", "Height", "ByteCode", "Info")

    val blocks = stackTraces.flatMap { st =>
      val header = center(st.header) + "\n\n" + legend
      header :: st.content :: Nil
    }

    print(makeBox(header, blocks))
  }

  private def printTokens(allTokens: List[List[Token]]) = {
    val legend = s"$Bold%-35s %-16s %s$Reset"
      .format("Text", "Token", "Position")
    val blocks = allTokens.flatMap { tokens =>
      val file = tokens.head.file
      val header = center(formatFileName(file)) + "\n\n" + legend
      val body = tokens.map(formatToken).mkString("\n")
      header :: body :: Nil
    }
    print(makeBox(header, blocks))
  }

  private def formatToken(t: Token) = {
    val tokenName = t.kind.getClass.getSimpleName.dropRight(1)
    val token = t.toString
    val trimmed = if (token.charCount >= 34) token.takeChars(30) + "..." else token
    val pos = Red(t.line) + ":" + Red(t.col) + " - " + Red(t.endLine) + ":" + Red(t.endCol)
    s"$Blue%-35s$Reset $Bold%-16s$Reset %s"
      .format(trimmed, tokenName, pos)
  }

}
