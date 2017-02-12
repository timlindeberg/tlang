

/**
  * Created by Tim Lindeberg on 4/2/2016.
  */

object TestMain {
  def main(args: Array[String]): Unit = {
    val arr = Array(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    println(arr.asInstanceOf[Array[Int]])
  }

  def splice(arr: Array[Int], start: Int, end: Int, step: Int) = {
    var i = start
    val x = end - start
    val count: Int = (x + step - 1) / step
    val newArr = Array.fill(count)(0)
    while (i < end) {
      newArr((i - start) / step) = arr(i)
      i += step
    }
    newArr
  }


}

trait T1 {

  override def toString = "T1"
}


trait T2 extends T1 {

  override def toString = "T1"
}


class Lol extends T1
