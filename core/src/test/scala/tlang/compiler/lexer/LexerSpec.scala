package tlang.compiler.lexer

import tlang.compiler.ast.Parser
import tlang.compiler.ast.Trees.CompilationUnit
import tlang.compiler.{ErrorTester, Pipeline, Tester}
import tlang.utils.Source

class LexerSpec extends ErrorTester {
  override def Name: String = "Lexer"
  override def Path: String = Tester.Resources + "lexer"
  override def Pipeline: Pipeline[Source, CompilationUnit] = Lexer andThen Parser
}