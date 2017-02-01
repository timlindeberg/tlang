package tcompiler.utils

import cafebabe.CodeHandler
import tcompiler.ast.Trees._
import tcompiler.lexer.Token

/**
  * Created by Tim Lindeberg on 2/1/2017.
  */

class InfoPrinter[-F, +T](stage: Pipeline[F, T], ctx: Context) {

  import ctx.colorizer._

  private val stageName            = stage.stageName.capitalize
  private val shouldPrint: Boolean = ctx.printCodeStages.contains(stageName.toLowerCase)


  def printCode[S >: T](output: S): Unit = {
    if (!shouldPrint)
      return

    output match {
      case (_: CompilationUnit) :: _ => printCompilationUnits(output.asInstanceOf[List[CompilationUnit]])
      case ((_: Token) :: _) :: _    => printTokens(output.asInstanceOf[List[List[Token]]])
      case _                         =>
    }
  }

  def printHeader(): Unit =
    if (shouldPrint)
      println(Bold("Output after ") + Blue(stageName) + Bold(":\n"))

  def printStacktrace(codeHandler: CodeHandler): Unit =
    if (shouldPrint)
      println(codeHandler.stackTrace(ctx.colorizer))


  private def printCompilationUnits(cus: List[CompilationUnit]) = {
    printHeader()
    val res = cus.map { cu => ctx.printer(cu) + "\n" }.mkString
    println(res)
  }

  private def printTokens(tokens: List[List[Token]]) = {
    printHeader()
    val res = tokens.map {_.map(formatToken).mkString("\n")}.mkString("\n\n") + "\n"
    println(res)
  }

  private def formatToken(t: Token) = {
    val tokenName = t.kind.getClass.getSimpleName.dropRight(1)
    val token = t.toString
    s"$Blue%-35s$Reset => $Bold%-15s$Reset $Bold[$Reset$Blue Pos$Reset: ($Red%s$Reset:$Red%s$Reset) - ($Red%s$Reset:$Red%s$Reset) $Bold]$Reset"
      .format(token, tokenName, t.line, t.col, t.endLine, t.endCol)
  }

}
