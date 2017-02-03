package tcompiler.utils

import cafebabe.StackTrace
import tcompiler.ast.Trees._
import tcompiler.lexer.Token
import tcompiler.utils.Extensions._

/**
  * Created by Tim Lindeberg on 2/1/2017.
  */

case class InfoPrinter[-F, +T](stage: Pipeline[F, T], ctx: Context) {

  import ctx.formatting._
  import ctx.formatting.colors._

  private val stageName            = stage.stageName.capitalize
  private val shouldPrint: Boolean = ctx.printCodeStages.contains(stageName.toLowerCase)
  val header: String = Bold("Output after stage ") + Blue(stageName)


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
    val blocks = cus.map { cu => ctx.printer(cu) }
    print(makeBox(header, blocks))
  }

  private def printStackTraces(stackTraces: List[StackTrace]) = {
    val blocks = stackTraces.flatMap { st => center(st.header) :: st.content :: Nil }
    print(makeBox(header, blocks))
  }

  private def printTokens(tokens: List[List[Token]]) = {
    val legend = s"$Bold%-35s %-16s %-12s %-12s$Reset"
      .format("Text", "Token", "Start", "End")
    val blocks = legend :: tokens.map {_.map(formatToken).mkString("\n")}
    print(makeBox(header, blocks))
  }

  private def formatToken(t: Token) = {
    val tokenName = t.kind.getClass.getSimpleName.dropRight(1)
    val token = t.toString
    val trimmed = if (token.charCount >= 34) token.takeChars(30) + "..." else token
    val start = s"${Red(t.line)}:${Red(t.col)}"
    val end = s"${Red(t.endLine)}:${Red(t.endCol)}"
    s"$Blue%-35s$Reset $Bold%-16s$Reset %-28s %-28s"
      .format(trimmed, tokenName, start, end)
  }

}
