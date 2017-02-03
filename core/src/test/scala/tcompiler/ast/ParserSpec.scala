package tcompiler.ast

import java.io.File

import tcompiler.ast.Trees.CompilationUnit
import tcompiler.lexer.Lexer
import tcompiler.utils.Pipeline
import tcompiler.{ErrorTester, Tester}

class ParserSpec extends ErrorTester {
  override def Name: String = "Parser"
  override def Path: String = Tester.Resources + "ast"
  override def Pipeline: Pipeline[Set[File], List[CompilationUnit]] = Lexer andThen Parser
}