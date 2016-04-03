package tcompiler.code

import java.io._

import org.scalatest._
import tcompiler.TestUtils
import tcompiler.analyzer.{NameAnalysis, Symbols, TypeChecking}
import tcompiler.ast._
import tcompiler.lexer.Lexer
import tcompiler.utils.{CompilationException, Context}


class CodeSpec extends FlatSpec with Matchers with BeforeAndAfter {

  import TestUtils._

  before {
    Symbols.ID.reset()
  }

  behavior of "Correct Programs"
  programFiles(Resources + "programs/valid").foreach(test(_, testPositive))

  def test(file: File, testFunction: File => Unit): Unit = {
    if (file.isDirectory)
      programFiles(file.getPath).foreach(test(_, testFunction))
    else
      it should file.getName.toString in testFunction(file)
  }

  def testPositive(file: File): Unit = {
    val ctx = new Context(reporter = new tcompiler.utils.Reporter, file = file, outDir = Some(new File("./gen/" + file.getName + "/")))

    try {
      val program = (Lexer andThen Parser andThen NameAnalysis andThen TypeChecking).run(ctx)(ctx.file)

      hasTypes(program) should be(true)
      ctx.reporter.hasErrors should be(false)

      CodeGeneration.run(ctx)(program)

      val res = lines(executeTProgram(file, "./gen/"))
      val sol = parseSolutions(file)
      assertCorrect(res, sol, "")
    } catch {
      case t: CompilationException  => fail("Compilation failed:\n" + t.getMessage)
      case t: FileNotFoundException => fail("Invalid test, file not found: " + file.getPath)
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
