package tcompiler.modification

import tcompiler.ast.Parser
import tcompiler.ast.Trees.Program
import tcompiler.lexer.Lexer
import tcompiler.{ErrorTester, TestUtils}

/**
 * Created by Tim Lindeberg on 4/4/2016.
 */
class ImportsSpec extends ErrorTester {
  override def Name: String = "Imports"
  override def Path: String = TestUtils.Resources + "modification/imports"
  override def Pipeline = Lexer andThen Parser andThen Templates andThen Imports
}