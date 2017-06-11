package tlang.compiler.analyzer

import tlang.compiler.ast.Parsing
import tlang.compiler.ast.Trees.CompilationUnit
import tlang.compiler.lexer.Lexing
import tlang.compiler.modification.Templating
import tlang.compiler.{ErrorTester, CompilerPhase, Tester}
import tlang.utils.Source

class TypingSpec extends ErrorTester {
  override def Name: String = "Type Checking"
  override def Path: String = Tester.Resources + "analyzer/type"
  override def Pipeline: CompilerPhase[Source, CompilationUnit] = Lexing andThen Parsing andThen Templating andThen Naming andThen Typing
}