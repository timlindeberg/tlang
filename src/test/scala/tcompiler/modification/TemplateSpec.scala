package tcompiler.modification

import tcompiler.ast.Parser
import tcompiler.lexer.Lexer
import tcompiler.{ErrorTester, TestUtils}

/**
 * Created by Tim Lindeberg on 4/4/2016.
 */
class TemplateSpec extends ErrorTester {
  override def Name: String = "Templates"
  override def Path: String = TestUtils.Resources + "modification/templates"
  override def Pipeline = Lexer andThen Parser andThen Templates

}