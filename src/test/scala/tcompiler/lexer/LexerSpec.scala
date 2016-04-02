package tcompiler.lexer

import tcompiler.{TestUtils, ErrorTester}

class LexerSpec extends ErrorTester {
  override def Name: String = "Lexer"
  override def Path: String = TestUtils.Resources + "lexer"
}