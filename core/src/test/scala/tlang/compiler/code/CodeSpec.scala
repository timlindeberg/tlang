package tlang.compiler.code


import tlang.testutils.{Tester, ValidTester}


class CodeSpec extends ValidTester {
  override def Name: String = "Code"
  override def Path: String = Tester.Resources + "code"
}
