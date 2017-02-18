package tlang.compiler.ast

import tlang.compiler.ast.Trees.CompilationUnit
import tlang.compiler.lexer.Lexer
import tlang.compiler.{ErrorTester, Pipeline, Tester}
import tlang.utils.Source

class ParserSpec extends ErrorTester {
  override def Name: String = "Parser"
  override def Path: String = Tester.Resources + "ast"
  override def Pipeline: Pipeline[Source, CompilationUnit] = Lexer andThen Parser
}