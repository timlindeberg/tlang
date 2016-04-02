package tcompiler.ast

import tcompiler.{ErrorTester, TestUtils}

class ParserSpec extends ErrorTester {
  override def Name: String = "Parser"
  override def Path: String = TestUtils.Resources + "ast"
}