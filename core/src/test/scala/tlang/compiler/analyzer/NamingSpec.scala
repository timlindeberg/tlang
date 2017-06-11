package tlang.compiler.analyzer

import tlang.compiler.ast.Parsing
import tlang.compiler.ast.Trees.CompilationUnit
import tlang.compiler.lexer.Lexing
import tlang.compiler.modification.Templating
import tlang.compiler.{ErrorTester, CompilerPhase, Tester}
import tlang.utils.Source

class NamingSpec extends ErrorTester {
  override def Name: String = "Name Analysis"
  override def Path: String = Tester.Resources + "analyzer/name"
  override def Pipeline: CompilerPhase[Source, CompilationUnit] = Lexing andThen Parsing andThen Templating andThen Naming
}