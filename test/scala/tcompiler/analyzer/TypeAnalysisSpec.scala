package tcompiler.analyzer

import tcompiler.ast.Parser
import tcompiler.lexer.Lexer
import tcompiler.modification.Templates
import tcompiler.{ErrorTester, TestUtils}

class TypeAnalysisSpec extends ErrorTester {
  override def Name: String = "Type Analysis"
  override def Path: String = TestUtils.Resources + "analyzer/type"
  override def Pipeline = Lexer andThen Parser andThen Templates andThen NameAnalysis andThen TypeChecking

}