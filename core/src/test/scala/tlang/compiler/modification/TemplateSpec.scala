package tlang.compiler.modification

import tlang.compiler.ast.Parser
import tlang.compiler.ast.Trees.CompilationUnit
import tlang.compiler.lexer.Lexer
import tlang.compiler.{ErrorTester, Pipeline, Tester}
import tlang.utils.Source


/**
  * Created by Tim Lindeberg on 4/4/2016.
  */
class TemplateSpec extends ErrorTester {
  override def Name: String = "Templates"
  override def Path: String = Tester.Resources + "modification/templates"
  override def Pipeline: Pipeline[Source, CompilationUnit] = Lexer andThen Parser andThen Templates
}