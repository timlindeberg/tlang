package tcompiler.code

import tcompiler.{TestUtils, ValidTester}



class CodeSpec extends ValidTester {

  override def Name: String = "Code"
  override def Path: String = TestUtils.Resources + "code/ArrayLength.kool"

}
