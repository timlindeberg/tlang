package tcompiler.analyzer

import tcompiler.{ErrorTester, TestUtils}
import tcompiler.ast.Parser
import tcompiler.lexer.Lexer
import tcompiler.modification.Templates

/**
  * Created by timlindeberg on 25/07/16.
  */
class FlowAnalysisSpec extends ErrorTester {
  override def Name: String = "Flow Analysis"
  override def Path: String = TestUtils.Resources + "analyzer/flow"
  override def Pipeline = Lexer andThen Parser andThen Templates andThen NameAnalysis andThen TypeChecking andThen FlowAnalysis
}