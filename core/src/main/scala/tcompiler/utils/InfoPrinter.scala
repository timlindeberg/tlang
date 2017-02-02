package tcompiler.utils

import cafebabe.CodeHandler
import tcompiler.ast.Trees._
import tcompiler.lexer.Token

/**
  * Created by Tim Lindeberg on 2/1/2017.
  */

case class InfoPrinter[-F, +T](stage: Pipeline[F, T], ctx: Context) {

  import ctx.formatting._
  import ctx.formatting.colorizer._

  private val stageName            = stage.stageName.capitalize
  private val shouldPrint: Boolean = ctx.printCodeStages.contains(stageName.toLowerCase)
  val header: String = Bold("Output after stage ") + Blue(stageName)


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
      print(makeHeader(header))

  def printEnd(): Unit =
    if (shouldPrint)
      print(bottom)

  def printStacktrace(codeHandler: CodeHandler): Unit =
    if (shouldPrint) {
      val code = codeHandler.stackTrace(ctx.formatting.colorizer)
      print(makeBlock(code))
    }

  private def printCompilationUnits(cus: List[CompilationUnit]) = {
    val blocks = cus.map { cu => ctx.printer(cu) }
    print(makeBox(header, blocks))
  }

  private def printTokens(tokens: List[List[Token]]) = {
    val blocks = tokens.map {_.map(formatToken).mkString("\n")}
    print(makeBox(header, blocks))
  }

  private def formatToken(t: Token) = {
    val tokenName = t.kind.getClass.getSimpleName.dropRight(1)
    val token = t.toString
    s"$Blue%-20s$Reset => $Bold%-15s$Reset $Bold[$Reset$Blue Pos$Reset: ($Red%s$Reset:$Red%s$Reset) - ($Red%s$Reset:$Red%s$Reset) $Bold]$Reset"
      .format(token, tokenName, t.line, t.col, t.endLine, t.endCol)
  }

}
