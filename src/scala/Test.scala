/**
 * Created by Tim Lindeberg on 4/2/2016.
 */
class Test extends Lol1 with Lol2 {
  override def anotherMethod(): Int = 1
  override def aMethod(i: Int): Int = 1
}

trait Lol1 {

  var anInt: Int = 1

  def anotherMethod(): Int

}

trait Lol2 {

  var anInt2: Int = 2

  def aMethod(i: Int): Int

  def anImplementedMethod(i: Int): Int = {
    aMethod(i) + anInt2
  }

}
