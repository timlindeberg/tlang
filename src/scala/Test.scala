/**
 * Created by Tim Lindeberg on 4/2/2016.
 */

object TestMain {
  def main(args: Array[String]) {
    val a = new A
    val b = a.anImplementedMethod()
    println(b)
  }
}

class A extends Test1 with Test2 {
  override def anImplementedMethod(): Int = super[Test1].anImplementedMethod()
}


trait Test1 {

  def anImplementedMethod(): Int = 1

}


trait Test2 {

  def anImplementedMethod(): Int = 2

}

