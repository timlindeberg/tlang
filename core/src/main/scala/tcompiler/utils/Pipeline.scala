package tcompiler
package utils

import tcompiler.ast.Printer
import tcompiler.ast.Trees.CompilationUnit

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

    def printCode[S >: T](output: S): Unit = {
      val s = stage.stageName
      if (!ctx.printCodeStages.contains(s))
        return

      output match {
        case list: List[_] if list.nonEmpty =>
          list.head match {
            case _: CompilationUnit =>
              val stageName = Blue(s.capitalize)
              println(s"${Bold}Output after $Reset$stageName:\n")
              list.map(_.asInstanceOf[CompilationUnit]) foreach {
                cu => println(Printer(cu, ctx.useColor) + "\n")
              }
            case _                  =>
          }
        case _                              =>
      }
    }

  }

}
