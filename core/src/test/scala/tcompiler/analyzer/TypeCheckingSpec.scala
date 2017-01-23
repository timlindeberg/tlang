package tcompiler.analyzer

import java.io.File

import tcompiler.ast.Parser
import tcompiler.ast.Trees.CompilationUnit
import tcompiler.lexer.Lexer
import tcompiler.modification.Templates
import tcompiler.utils.Pipeline
import tcompiler.{ErrorTester, Tester}

class TypeCheckingSpec extends ErrorTester {
  override def Name: String = "Type Checking"
  override def Path: String = Tester.Resources + "analyzer/type"
  override def Pipeline: Pipeline[List[File], List[CompilationUnit]] = Lexer andThen Parser andThen Templates andThen NameAnalysis andThen TypeChecking
}