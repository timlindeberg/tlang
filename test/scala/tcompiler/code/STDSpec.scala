package tcompiler.code

import tcompiler.{TestUtils, ValidTester}



class STDSpec extends ValidTester {

  override def Name: String = "Standard Lib"
  override def Path: String = TestUtils.Resources + "stdtests/VectorTest.kool"

}
