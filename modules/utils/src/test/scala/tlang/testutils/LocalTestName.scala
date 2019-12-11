package tlang
package testutils

object LocalTestName {
  val LocalNameSeparator: String = " "
}

trait LocalTestName {

  import LocalTestName._

  private val _localTestNames: ThreadLocal[List[String]] = new ThreadLocal[List[String]]()

  // For readability mostly. Appends the description to the snapshot name if
  // a snapshot test is executed within the block
  def test[U](description: String, ignore: Boolean = false)(testFun: => U): Unit = {
    if (ignore)
      return

    withLocalName(description) { testFun }
  }

  def localTestName: String = {
    val local = localTestNames
    if (local.isEmpty) "" else local.reverse.mkString(LocalNameSeparator, LocalNameSeparator, "")
  }

  private def withLocalName[U](name: String)(testFun: => U): Unit = {
    val testNames = name :: localTestNames
    localTestNames = testNames
    try { testFun } finally { localTestNames = testNames.tail }
  }

  private def localTestNames: List[String] = Option(_localTestNames.get()).getOrElse(Nil)
  private def localTestNames_=(names: List[String]): Unit = _localTestNames.set(names)
}
