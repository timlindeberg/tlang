/**
 * Created by Tim Lindeberg on 4/2/2016.
 */

object TestMain {
  def main(args: Array[String]) {
    val a = new A
    val b = a.anImplementedMethod(1)
  }
}

class A extends Test

trait Test {

  def anImplementedMethod(i: Int): Int = 1

}
