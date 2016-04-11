package tcompiler.analyzer

import tcompiler.ast.Parser
import tcompiler.lexer.Lexer
import tcompiler.modification.{Imports, Templates}
import tcompiler.{ErrorTester, TestUtils}

class NameAnalysisSpec extends ErrorTester {
  override def Name: String = "Name Analysis"
  override def Path: String = TestUtils.Resources + "analyzer/name/BreakContinueOutsideLoop.kool"
  override def Pipeline = Lexer andThen Parser andThen Templates andThen Imports andThen NameAnalysis
}