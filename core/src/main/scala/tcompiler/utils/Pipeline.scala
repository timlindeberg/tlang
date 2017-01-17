package tcompiler
package utils

import tcompiler.ast.Printer
import tcompiler.ast.Trees.CompilationUnit
import tcompiler.lexer.Token

abstract class Pipeline[-F, +T] {
  self =>


  def run(ctx: Context)(v: F): T

  def stageName: String = getClass.getSimpleName.dropRight(1).toLowerCase

  def andThen[G](thenn: Pipeline[T, G]): Pipeline[F, G] = new Pipeline[F, G] {
    def run(ctx: Context)(v: F): G = {
      val first = self.execute(ctx)(v)
      ctx.reporter.terminateIfErrors()
      val second = thenn.execute(ctx)(first)
      ctx.reporter.terminateIfErrors()
      second
    }
  }

  private def execute(ctx: Context)(v: F): T = {
    val infoPrinter = new InfoPrinter(this, ctx)
    val (output, time) = HelpMethods.timed {run(ctx)(v)}
    if (Main.CompilerStages.contains(this))
      ctx.executionTimes += this -> time
    infoPrinter.printCode(output)
    output
  }

  class InfoPrinter(stage: Pipeline[F, T], ctx: Context) extends Colored {

    override val useColor: Boolean = ctx.useColor

    private val stageName = stage.stageName.capitalize

    def printHeader() = println(s"${Bold}Output after $Reset$Blue$stageName$Reset:\n")

    def printCode[S >: T](output: S): Unit = {
      if (!ctx.printCodeStages.contains(stage.stageName))
        return

      output match {
        case (_: CompilationUnit) :: _ => printCompilationUnits(output.asInstanceOf[List[CompilationUnit]])
        case ((_: Token) :: _) :: _    => printTokens(output.asInstanceOf[List[List[Token]]])
        case _                         =>
      }
    }

    private def printCompilationUnits(cus: List[CompilationUnit]) = {
      printHeader()
      val res = cus map { cu => Printer(cu, ctx.useColor) + "\n" }
      println(res)
    }

    private def printTokens(tokens: List[List[Token]]) = {
      printHeader()
      val res = tokens.map {_.map(formatToken).mkString("\n")}.mkString("\n\n")
      println(res)
    }

    private def formatToken(t: Token) = {
      val name = t.kind.getClass.getSimpleName.dropRight(1)
      val sign = t.toString
      val desc = s"$name"
      s"$Blue%-10s$Reset => $Bold%-10s$Reset $Bold[$Reset$Blue Pos$Reset: ($Red%s$Reset:$Red%s$Reset) - ($Red%s$Reset:$Red%s$Reset) $Bold]$Reset".format(sign, desc, t.line, t.col, t.endLine, t.endCol)
    }

  }

}
