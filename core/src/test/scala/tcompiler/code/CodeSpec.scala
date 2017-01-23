package tcompiler.code

import tcompiler.{Tester, ValidTester}


class CodeSpec extends ValidTester {
  override def Name: String = "Code"
  override def Path: String = Tester.Resources + "code"
}
