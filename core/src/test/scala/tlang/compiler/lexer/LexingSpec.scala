package tlang.compiler.lexer

import tlang.compiler.CompilerPhase
import tlang.compiler.ast.Parsing
import tlang.compiler.ast.Trees.CompilationUnit
import tlang.testutils.{ErrorTester, Tester}
import tlang.utils.Source

class LexingSpec extends ErrorTester {
  override def Name: String = "Lexer"
  override def Path: String = Tester.Resources + "lexer"
  override def Pipeline: CompilerPhase[Source, CompilationUnit] = Lexing andThen Parsing
}
