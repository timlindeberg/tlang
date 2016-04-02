package tcompiler.code

import java.io._

import org.scalatest._
import tcompiler.TestUtils
import tcompiler.analyzer.{NameAnalysis, Symbols, TypeChecking}
import tcompiler.ast._
import tcompiler.lexer.Lexer
import tcompiler.utils.{CompilationException, Context}

class CodeSpec extends FlatSpec with Matchers with BeforeAndAfter {


  before {
    Symbols.ID.reset()
  }

  //behavior of "Correct Programs"
  //TestUtils.programFiles(TestUtils.resources + "programs/valid").foreach(test(_, testPositive))

  behavior of "Incorrect Programs"
  TestUtils.programFiles(TestUtils.resources + "programs/invalid").foreach(test(_, testNegative))

  def test(file: File, testFunction: File => Unit): Unit = {
    if (file.isDirectory)
      TestUtils.programFiles(file.getPath).foreach(test(_, testFunction))
    else
      it should file.getName.toString in testFunction(file)
  }

  def testPositive(file: File): Unit = {
    val ctx = new Context(reporter = new tcompiler.utils.Reporter, file = file, outDir = Some(new File("./gen/" + file.getName + "/")))

    try {
      val program = (Lexer andThen Parser andThen NameAnalysis andThen TypeChecking).run(ctx)(ctx.file)

      TestUtils.HasTypes(program) should be(true)
      ctx.reporter.hasErrors should be(false)

      CodeGeneration.run(ctx)(program)

      val res = TestUtils.lines(TestUtils.executeTProgram(file, "./gen/"))
      val sol = TestUtils.parseSolutions(file)
      assertCorrect(res, sol)
    } catch {
      case t: CompilationException  => fail("Compilation failed:\n" + t.getMessage)
      case t: FileNotFoundException => fail("Invalid test, file not found: " + file.getPath)
      case t: RuntimeException      => fail("Test failed, program execution failed.")
      case _                        => fail("Test failed!")
    }
  }

  def testNegative(file: File): Unit = {
    if (file.isDirectory) {
      TestUtils.programFiles(file.getPath).foreach(testNegative)
      return
    }

    val ctx = new Context(reporter = new tcompiler.utils.Reporter(quiet = true), file = file, outDir = Some(new File("./gen/" + file.getName + "/")))
    val expectedErrors = TestUtils.parseSolutions(file)

    try{
      val program = (Lexer andThen Parser andThen NameAnalysis andThen TypeChecking).run(ctx)(ctx.file)
      fail("Test failed: No compilation exception was thrown!")
    } catch {
      case t: CompilationException =>
        val errorCodes = TestUtils.parseErrorCodes(t.getMessage)
        assertCorrect(errorCodes, expectedErrors)
      case t: Throwable => fail("Test failed: " + t.getMessage)
    }
  }

  private def assertCorrect(res: List[String], sol: List[String]) = {
    assert(res.length == sol.length, "Different amount of results and expected results.")

    printResultsVersusSolution(res, sol)
    flattenTuple(res.zip(sol).zipWithIndex).foreach {
      case (r, s, i) =>
        assert(r.trim == s.trim, s": error on test ${i + 1}: expected '${s.trim}', found: '${r.trim}'")
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
