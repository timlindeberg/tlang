package tcompiler

import java.io.File

import org.scalatest.{BeforeAndAfter, FlatSpec, Matchers}
import tcompiler.analyzer.{NameAnalysis, Symbols, TypeChecking}
import tcompiler.ast.Parser
import tcompiler.lexer.Lexer
import tcompiler.utils.{CompilationException, Context}

/**
 * Created by Tim Lindeberg on 4/2/2016.
 */
abstract class ErrorTester extends FlatSpec with Matchers with BeforeAndAfter {

  def Name: String
  def Path: String

  before {
    Symbols.ID.reset()
  }

  behavior of Name
  TestUtils.programFiles(Path).foreach(test)

  def test(file: File): Unit =
    if (file.isDirectory)
      TestUtils.programFiles(file.getPath).foreach(test)
    else
      it should file.getName.toString in testFile(file)

  private def testFile(file: File): Unit = {
    val ctx = new Context(reporter = new tcompiler.utils.Reporter(quiet = true), file = file, outDir = Some(new File("./gen/" + file.getName + "/")))
    val expectedErrors = TestUtils.parseSolutions(file)

    try {
      (Lexer andThen Parser andThen NameAnalysis andThen TypeChecking).run(ctx)(ctx.file)
      fail("Test failed: No compilation exception was thrown!")
    } catch {
      case t: CompilationException =>
        val errorCodes = TestUtils.parseErrorCodes(t.getMessage)
        assertCorrect(errorCodes, expectedErrors, t.getMessage)
      case t: Throwable => fail("Test failed: " + t.getMessage)
    }
  }

  private def assertCorrect(res: List[String], sol: List[String], errorMsg: String) = {
    assert(res.length == sol.length, "Different amount of results and expected results.\n\n" + errorMsg)

    printResultsVersusSolution(res, sol)
    flattenTuple(res.zip(sol).zipWithIndex).foreach {
      case (r, s, i) =>
        assert(r.trim == s.trim, s": error on test ${i + 1} \n $errorMsg")
    }
  }

  private def flattenTuple[A, B, C](t: List[((A, B), C)]): List[(A, B, C)] = t.map(x => (x._1._1, x._1._2, x._2))

  private def printResultsVersusSolution(res: List[String], sol: List[String]) = {
    val Length = 20
    val PF = "%-" + Length + "s%-" + Length + "s\n"
    printSeperator(100)
    printf(PF, "Result:", "Solution:")
    res.zip(sol).foreach { case (r, s) => printf(PF, r, s) }
  }

  private def printSeperator(length: Int) = {
    (0 until length).foreach(_ => print("-"))
    println()
  }

}
