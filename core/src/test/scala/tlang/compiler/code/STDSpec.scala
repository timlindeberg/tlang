package tlang.compiler.code

import tlang.testutils.{Tester, ValidTester}


class STDSpec extends ValidTester {
  override def Name: String = "Standard Lib"
  override def Path: String = Tester.Resources + "stdtests"
}
