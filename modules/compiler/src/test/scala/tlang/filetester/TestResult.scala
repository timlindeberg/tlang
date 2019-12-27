package tlang
package filetester

case class TestResult(success: Boolean, reason: String, extraInfo: List[String]) {
  def description: String = {
    if (extraInfo.isEmpty)
      return reason
    reason + NL + extraInfo.mkString(NL)
  }
}
case class TestFailedException(reason: String, extraBoxes: List[String]) extends RuntimeException
