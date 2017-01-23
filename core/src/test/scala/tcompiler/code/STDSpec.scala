package tcompiler.code

import tcompiler.{Tester, ValidTester}


class STDSpec extends ValidTester {
  override def Name: String = "Standard Lib"
  override def Path: String = Tester.Resources + "stdtests"
}
