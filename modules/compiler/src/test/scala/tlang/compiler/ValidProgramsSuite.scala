package tlang
package compiler

import better.files.File
import org.scalatest.ParallelTestExecution
import tlang.compiler.analyzer.Symbols.Symbolic
import tlang.compiler.analyzer.Types.Typed
import tlang.compiler.ast.TreePrinter
import tlang.compiler.ast.Trees.{CompilationUnit, Tree}
import tlang.compiler.messages.CompilationException
import tlang.compiler.output.ErrorMessageOutput
import tlang.formatting.grid.{Column, TruncatedColumn}
import tlang.testutils.TestConstants
import tlang.testutils.TestConstants._
import tlang.utils.{DefaultProgramExecutor, FileSource, Logging}

import scala.runtime.ScalaRunTime

class ValidProgramsSuite extends CompilerIntegrationTestSpec with ParallelTestExecution with Logging {

  override val suiteName: String = "Valid Programs"

  testFiles(s"$Resources/validtests", testValidProgram)

  def testValidProgram(file: File): Unit = {
    val ctx = testContext(Some(file))

    val programExecutor = DefaultProgramExecutor(ctx.allClassPaths)

    val sources = FileSource(file) :: Nil
    val cus = try {
      Main.FrontEnd.execute(ctx)(sources)
    } catch {
      case e: CompilationException =>
        ctx.output += ErrorMessageOutput(e.messages)
        fail("Compilation failed")
      case e: Throwable            =>
        error"Unknown execution error: $e"
        throw e
    }


    ctx.reporter.hasErrors shouldBe false
    cus foreach verifyTypesAndSymbols

    Main.GenerateCode.execute(ctx)(cus)

    if (Verbose)
      printExecutionTimes(file, ctx)

    val res = programExecutor(file)
    res.exception.ifDefined { e => fail(s"Program execution failed with exception: ${ e.stackTrace }") }
    val resLines = lines(res.output)
    val sol = parseSolutions(file)
    assertCorrect(resLines, sol)
  }

  private def verifyTypesAndSymbols(cu: CompilationUnit): Unit = {
    def missing(t: Tree, missing: String) = {
      val treePrinter = new TreePrinter
      val treeRepr = ScalaRunTime._toString(t)

      TestConstants.TestFormatter.grid
        .header(s"Tree $treeRepr has a missing $missing")
        .row(Column, TruncatedColumn, Column, Column, TruncatedColumn)
        .columnHeaders("Line", "Tree", "Reference", "Symbol", "Type")
        .contents(treePrinter(cu))
        .print()

      fail(s"Tree $treeRepr does not have a $missing.")
    }

    cu foreach { tree: Tree =>
      tree match {
        case s: Symbolic[_] if !s.hasSymbol => missing(tree, "symbol")
        case _                              =>
      }
      tree match {
        case t: Typed if !t.hasType => missing(tree, "type")
        case _                      =>
      }
    }
  }

  private def lines(str: String): List[String] = str.split("\\r?\\n").map(_.trim).toList

  private def parseSolutions(file: File): List[(Int, String)] =
    FileSource
      .getText(file)
      .lines
      .zipWithIndex
      .collect { case (SolutionRegex(line), lineNumber) => (lineNumber + 1, line.trim) }
      .toList

  private def assertCorrect(results: List[String], solutions: List[(Int, String)]): Unit = {
    def extraInfo(i: Int) = NL + formatTestFailedMessage(i + 1, results, solutions.map(_._2))

    results
      .zip(solutions)
      .zipWithIndex
      .foreach {
        case ((res, (line, sol)), i) if res != sol => fail(s"Expected '$sol' but found '$res' at line $line ${ extraInfo(i) }")
        case _                                     =>
      }
    if (results.lengthCompare(solutions.length) != 0) {
      fail(s"Expected ${ solutions.length } lines but ${ results.length } were output ${ extraInfo(-1) }")
    }
  }

}
