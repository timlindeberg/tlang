package tcompiler.modification

import java.io.File

import tcompiler.ast.Parser
import tcompiler.ast.Trees.CompilationUnit
import tcompiler.lexer.Lexer
import tcompiler.utils.Pipeline
import tcompiler.{ErrorTester, Tester}


/**
  * Created by Tim Lindeberg on 4/4/2016.
  */
class TemplateSpec extends ErrorTester {
  override def Name: String = "Templates"
  override def Path: String = Tester.Resources + "modification/templates"
  override def Pipeline: Pipeline[List[File], List[CompilationUnit]] = Lexer andThen Parser andThen Templates
}