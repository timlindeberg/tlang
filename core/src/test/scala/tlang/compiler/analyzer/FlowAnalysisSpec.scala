package tlang.compiler.analyzer

import tlang.compiler.ast.Parser
import tlang.compiler.ast.Trees.CompilationUnit
import tlang.compiler.lexer.Lexer
import tlang.compiler.modification.Templates
import tlang.compiler.{ErrorTester, Pipeline, Tester}
import tlang.utils.Source

/**
  * Created by timlindeberg on 25/07/16.
  */
class FlowAnalysisSpec extends ErrorTester {
  override def Name: String = "Flow Analysis"
  override def Path: String = Tester.Resources + "analyzer/flow"
  override def Pipeline: Pipeline[Source, CompilationUnit] = Lexer andThen Parser andThen Templates andThen NameAnalysis andThen TypeChecking andThen FlowAnalysis
}