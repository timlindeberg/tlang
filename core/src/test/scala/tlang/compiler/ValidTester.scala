package tlang.compiler

import java.io.{File, FileNotFoundException}

import tlang.compiler.analyzer.{FlowAnalysis, NameAnalysis, TypeChecking}
import tlang.compiler.ast.Parser
import tlang.compiler.ast.Trees.CompilationUnit
import tlang.compiler.code.{CodeGeneration, Desugaring}
import tlang.compiler.error.CompilationException
import tlang.compiler.lexer.Lexer
import tlang.compiler.modification.Templates
import tlang.compiler.utils.ProgramExecutor
import tlang.utils.{FileSource, Source}


/**
  * Created by Tim Lindeberg on 4/11/2016.
  */
trait ValidTester extends Tester {

  import Tester._

  val programExecutor = ProgramExecutor()

  override def Pipeline: Pipeline[Source, CompilationUnit] =
    Lexer andThen Parser andThen Templates andThen NameAnalysis andThen TypeChecking andThen FlowAnalysis

  def testFile(file: File): Unit = {
    val ctx = getTestContext(Some(file))

    try {
      val sources = FileSource(file) :: Nil
      val cus = Pipeline.run(ctx)(sources)

      ctx.reporter.hasErrors should be(false)

      val compilation = Desugaring andThen CodeGeneration
      compilation.run(ctx)(cus)
      val res = programExecutor(ctx, file).getOrElse(fail(s"Test timed out!"))
      val resLines = lines(res)
      val sol = parseSolutions(file)
      assertCorrect(resLines, sol)
    } catch {
      case t: CompilationException  =>
        println(t.getMessage)
        fail("Compilation failed")
      case _: FileNotFoundException => fail(s"Invalid test, file not found: ${file.getPath}")
    }
  }

  private def lines(str: String): List[String] = str.split("\\r?\\n").map(_.trim).toList

  private def parseSolutions(file: File): List[String] = {
    val fileName = file.getPath
    io.Source.fromFile(fileName).getLines().collect { case SolutionRegex(line) => line.trim }.toList
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
