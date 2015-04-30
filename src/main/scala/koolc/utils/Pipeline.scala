package koolc
package utils

abstract class Pipeline[-F, +T] {
  self =>

  def andThen[G](thenn: Pipeline[T, G]): Pipeline[F, G] = new Pipeline[F, G] {
    def run(ctx: Context)(v: F): G = {
      val first = self.run(ctx)(v)
      ctx.reporter.terminateIfErrors
      thenn.run(ctx)(first)
    }
  }

  def run(ctx: Context)(v: F): T
}
