package tcompiler
package utils

abstract class Pipeline[-F, +T] {
  self =>


  def run(ctx: Context)(v: F): T

  val stageName: String = getClass.getSimpleName.dropRight(1).toLowerCase

  def andThen[G](thenn: Pipeline[T, G]): Pipeline[F, G] = new Pipeline[F, G] {
    def run(ctx: Context)(v: F): G = {
      val first = self.execute(ctx)(v)
      val second = thenn.execute(ctx)(first)
      second
    }
  }

  private def execute(ctx: Context)(v: F): T = {
    val infoPrinter = OutputPrinter(this, ctx)
    val (output, time) = Helpers.timed {run(ctx)(v)}
    if (Main.CompilerStages.contains(this))
      ctx.executionTimes += this -> time
    infoPrinter.printCode(output)
    ctx.reporter.terminateIfErrors()
    output
  }
}
