package tcompiler.lexer

import tcompiler.{ErrorTester, TestUtils}
import tcompiler.ast.Parser

class LexerSpec extends ErrorTester {
  override def Name: String = "Lexer"
  override def Path: String = TestUtils.Resources + "lexer"
  override def Pipeline = Lexer andThen Parser

}