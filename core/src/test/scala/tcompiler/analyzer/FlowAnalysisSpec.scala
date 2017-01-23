package tcompiler.analyzer

import java.io.File

import tcompiler.ast.Parser
import tcompiler.ast.Trees.CompilationUnit
import tcompiler.lexer.Lexer
import tcompiler.modification.Templates
import tcompiler.utils.Pipeline
import tcompiler.{ErrorTester, Tester}

/**
  * Created by timlindeberg on 25/07/16.
  */
class FlowAnalysisSpec extends ErrorTester {
  override def Name: String = "Flow Analysis"
  override def Path: String = Tester.Resources + "analyzer/flow"
  override def Pipeline: Pipeline[List[File], List[CompilationUnit]] = Lexer andThen Parser andThen Templates andThen NameAnalysis andThen TypeChecking andThen FlowAnalysis
}