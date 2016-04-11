package tcompiler.lexer

import tcompiler.ast.Parser
import tcompiler.{ErrorTester, TestUtils}

class LexerSpec extends ErrorTester {
  override def Name: String = "Lexer"
  override def Path: String = TestUtils.Resources + "lexer"
  override def Pipeline = Lexer andThen Parser

}