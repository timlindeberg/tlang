

/**
 * Created by Tim Lindeberg on 4/2/2016.
 */

object TestMain {
  def main(args: Array[String]): Unit = {println(new Lol())}




}

trait T1 {

  override def toString = "T1"
}


trait T2 extends T1{

  override def toString = "T1"
}


class Lol extends T1
