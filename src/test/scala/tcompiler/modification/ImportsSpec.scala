package tcompiler.modification

import tcompiler.{TestUtils, ErrorTester}

/**
 * Created by Tim Lindeberg on 4/4/2016.
 */
class ImportsSpec extends ErrorTester {
  override def Name: String = "Imports"
  override def Path: String = TestUtils.Resources + "modification/imports"
}