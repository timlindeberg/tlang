package tlang.compiler

import better.files.File
import org.scalatest.ParallelTestExecution
import tlang.messages.{CompilationException, MessageType}
import tlang.testutils.TestConstants._
import tlang.utils.{FileSource, Logging, ProgramExecutor, Source}

import scala.concurrent.duration.Duration

class ValidProgramsSuite extends CompilerIntegrationTestSpec with ParallelTestExecution with Logging {

  override val suiteName: String = "Valid Programs"


  val TimeOut        = Duration(5, "sec")
  val ValidResources = s"$Resources/validtests"


  testCorrectOutputOfPrograms(s"$ValidResources/Programs")
  testCorrectOutputOfPrograms(s"$ValidResources/STD Lib")

  def testCorrectOutputOfPrograms(path: String): Unit = testFiles(path, testValidProgram)

  def testValidProgram(file: File): Unit = {
    val ctx = testContext(Some(file))
    val programExecutor = ProgramExecutor(ctx, TimeOut)

    val sources = FileSource(file) :: Nil
    val cus = try {
      Main.FrontEnd.execute(ctx)(sources)
    } catch {
      case e: CompilationException =>
        e.messages.print(MessageType.Error)
        fail("Compilation failed")
      case e: Throwable            =>
        error"Unknown execution error: $e"
        throw e
    }

    ctx.reporter.hasErrors shouldBe false

    Main.GenerateCode.execute(ctx)(cus)
    val res = programExecutor(file)
    val resLines = lines(res)
    val sol = parseSolutions(file)
    assertCorrect(resLines, sol)
  }

  private def lines(str: String): List[String] = str.split("\\r?\\n").map(_.trim).toList

  private def parseSolutions(file: File): List[(Int, String)] =
    Source
      .getText(file)
      .lines
      .zipWithIndex
      .collect { case (SolutionRegex(line), lineNumber) => (lineNumber + 1, line.trim) }
      .toList

  private def assertCorrect(results: List[String], solutions: List[(Int, String)]): Unit = {
    def extraInfo(i: Int) = formatTestFailedMessage(i + 1, results, solutions.map(_._2))

    results.zip(solutions).zipWithIndex.foreach {
      case ((res, (line, sol)), i) =>
        if (res != sol)
          fail(s"Expected '$sol' but found '$res' at line $line ${ extraInfo(i) }")
    }
    if (results.length != solutions.length) {
      fail(s"Expected ${ solutions.length } lines but ${ results.length } were output ${ extraInfo(-1) }")
    }
  }


}
