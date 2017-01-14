package tcompiler

import java.io.{File, FileNotFoundException}

import tcompiler.analyzer.{FlowAnalysis, NameAnalysis, TypeChecking}
import tcompiler.ast.Parser
import tcompiler.ast.Trees.CompilationUnit
import tcompiler.code.{CodeGeneration, Desugaring}
import tcompiler.lexer.Lexer
import tcompiler.modification.Templates
import tcompiler.utils.{CompilationException, Pipeline}

import scala.io.Source

/**
  * Created by Tim Lindeberg on 4/11/2016.
  */
trait ValidTester extends Tester {

  import TestUtils._

  override def Pipeline: Pipeline[List[File], List[CompilationUnit]] =
    Lexer andThen Parser andThen Templates andThen NameAnalysis andThen TypeChecking andThen FlowAnalysis

  def testFile(file: File): Unit = {
    val ctx = getTestContext(file)

    try {
      val cus = Pipeline.run(ctx)(List(file))

      ctx.reporter.hasErrors should be(false)

      val compilation = Desugaring andThen CodeGeneration
      compilation.run(ctx)(cus)
      val res = lines(executeTProgram(file))
      val sol = parseSolutions(file)
      assertCorrect(res, sol)
    } catch {
      case t: CompilationException  =>
        println(t.getMessage)
        fail("Compilation failed")
      case _: FileNotFoundException => fail(s"Invalid test, file not found: ${file.getPath}")
    }
  }

  private def parseSolutions(file: File): List[String] = {
    val fileName = file.getPath
    Source.fromFile(fileName).getLines().collect { case SolutionRegex(line) => line.trim }.toList
  }

  private def assertCorrect(res: List[String], sol: List[String]): Unit = {
    res.zip(sol).zipWithIndex.foreach {
      case ((r, s), i) =>
        val extraInfo = formatTestFailedMessage(i + 1, res, sol)
        if (r != s)
          fail(s"Expected '$s' but found '$r' $extraInfo")
    }
    if (res.length != sol.length) {
      val extraInfo = formatTestFailedMessage(-1, res, sol)
      fail(s"Expected ${sol.length} errors but ${res.length} were thrown $extraInfo")
    }
  }


}
