package tcompiler.ast

import java.io.File

import tcompiler.ast.Trees.CompilationUnit
import tcompiler.lexer.Lexer
import tcompiler.utils.Pipeline
import tcompiler.{ErrorTester, TestUtils}

class ParserSpec extends ErrorTester {
  override def Name: String = "Parser"
  override def Path: String = TestUtils.Resources + "ast"
  override def Pipeline: Pipeline[List[File], List[CompilationUnit]] = Lexer andThen Parser
}