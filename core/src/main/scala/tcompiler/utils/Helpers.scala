package tcompiler.utils

/**
  * Created by Tim Lindeberg on 1/6/2017.
  */
object Helpers {

  def timed[R](block: => R): (R, Double) = {
    val t0 = System.nanoTime()
    val res = block
    val t1 = System.nanoTime()
    (res, (t1 - t0) / 1000000000.0)
  }

  def clamp(x: Int, min: Int, max: Int): Int = Math.min(Math.max(x, min), max)

}
