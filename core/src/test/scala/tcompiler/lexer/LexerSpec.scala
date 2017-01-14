package tcompiler.lexer

import java.io.File

import tcompiler.{ErrorTester, TestUtils}
import tcompiler.ast.Parser
import tcompiler.ast.Trees.CompilationUnit
import tcompiler.utils.Pipeline

class LexerSpec extends ErrorTester {
  override def Name: String = "Lexer"
  override def Path: String = TestUtils.Resources + "lexer"
  override def Pipeline: Pipeline[List[File], List[CompilationUnit]] = Lexer andThen Parser
}