package tlang.compiler.modification

import tlang.compiler.CompilerPhase
import tlang.compiler.ast.Parsing
import tlang.compiler.ast.Trees.CompilationUnit
import tlang.compiler.lexer.Lexing
import tlang.testutils.{ErrorTester, Tester}
import tlang.utils.Source

class TemplatingSpec extends ErrorTester {
  override def Name: String = "Templates"
  override def Path: String = Tester.Resources + "modification/templates"
  override def Pipeline: CompilerPhase[Source, CompilationUnit] = Lexing andThen Parsing andThen Templating
}
