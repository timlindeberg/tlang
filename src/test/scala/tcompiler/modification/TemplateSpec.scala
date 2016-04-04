package tcompiler.modification

import tcompiler.{TestUtils, ErrorTester}

/**
 * Created by Tim Lindeberg on 4/4/2016.
 */
class TemplateSpec extends ErrorTester {
  override def Name: String = "Templates"
  override def Path: String = TestUtils.Resources + "modification/templates"
}