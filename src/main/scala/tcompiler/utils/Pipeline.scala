package tcompiler
package utils

abstract class Pipeline[-F, +T] {
  self =>

  def andThen[G](thenn: Pipeline[T, G]): Pipeline[F, G] = new Pipeline[F, G] {
    def run(ctx: Context)(v: F): G = {
      val first = self.run(ctx)(v)
      ctx.reporter.terminateIfErrors
      val second = thenn.run(ctx)(first)
      ctx.reporter.terminateIfErrors
      second
    }
  }

  def run(ctx: Context)(v: F): T
}
