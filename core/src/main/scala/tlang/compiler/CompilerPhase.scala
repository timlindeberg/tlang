package tlang.compiler

import tlang.Context
import tlang.utils.Extensions._
import tlang.utils.formatting.Formatting

abstract class CompilerPhase[F, T] {
  self =>

  val name: String = getClass.getSimpleName.dropRight(1).toLowerCase

  def description(formatting: Formatting): String
  def printDebugOutput(output: List[T], formatting: Formatting): Unit
  protected def run(ctx: Context)(v: List[F]): List[T]

  def andThen[G](thenn: CompilerPhase[T, G]): CompilerPhase[F, G] = new CompilerPhase[F, G] {
    def run(ctx: Context)(v: List[F]): List[G] = {
      val first = self.execute(ctx)(v)
      val second = thenn.execute(ctx)(first)
      second
    }

    override def description(formatting: Formatting): String = ""
    override def printDebugOutput(output: List[G], formatting: Formatting): Unit = {}

  }

  def execute(ctx: Context)(v: List[F]): List[T] = {
    val (output, time) = measureTime { run(ctx)(v) }
    if (Main.CompilerPhases.contains(this)) {

      if (!ctx.executionTimes.contains(this))
        ctx.executionTimes += this -> time

      if (name in ctx.printCodePhase)
        printDebugOutput(output, ctx.formatting)

    }
    ctx.reporter.terminateIfErrors()
    output
  }


}
