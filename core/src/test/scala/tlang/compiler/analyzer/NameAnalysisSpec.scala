package tlang.compiler.analyzer

import tlang.compiler.ast.Parser
import tlang.compiler.ast.Trees.CompilationUnit
import tlang.compiler.lexer.Lexer
import tlang.compiler.modification.Templates
import tlang.compiler.{ErrorTester, Pipeline, Tester}
import tlang.utils.Source

class NameAnalysisSpec extends ErrorTester {
  override def Name: String = "Name Analysis"
  override def Path: String = Tester.Resources + "analyzer/name"
  override def Pipeline: Pipeline[Source, CompilationUnit] = Lexer andThen Parser andThen Templates andThen NameAnalysis
}