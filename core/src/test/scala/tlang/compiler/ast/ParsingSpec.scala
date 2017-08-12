package tlang.compiler.ast

import tlang.compiler.CompilerPhase
import tlang.compiler.ast.Trees.CompilationUnit
import tlang.compiler.lexer.Lexing
import tlang.testutils.{ErrorTester, Tester}
import tlang.utils.Source

class ParsingSpec extends ErrorTester {
  override def Name: String = "Parser"
  override def Path: String = Tester.Resources + "ast"
  override def Pipeline: CompilerPhase[Source, CompilationUnit] = Lexing andThen Parsing
}
