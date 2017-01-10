package tcompiler.ast

import tcompiler.lexer.Lexer
import tcompiler.{ErrorTester, TestUtils}

class ParserSpec extends ErrorTester {
  override def Name: String = "Parser"
  override def Path: String = TestUtils.Resources + "ast"
  override def Pipeline = Lexer andThen Parser
}