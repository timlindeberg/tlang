package tlang.compiler

import java.io.{File, FileNotFoundException}

import org.scalatest.ParallelTestExecution
import tlang.Constants
import tlang.compiler.error.{CompilationException, MessageType}
import tlang.testutils.CompilerTestSpec
import tlang.utils.{FileSource, ProgramExecutor, Source}

import scala.concurrent.duration.Duration

class CompilerValidProgramSpec extends CompilerTestSpec with ParallelTestExecution {

  private val programExecutor = ProgramExecutor(Duration(5, "sec"))

  val ValidResources = s"$Resources/validtests"


  "Valid programs" - {
    testCorrectOutputOfPrograms(s"$ValidResources/programs")
  }

  "STD Lib" - {
    testCorrectOutputOfPrograms(s"$ValidResources/stdlib")
  }

  def testCorrectOutputOfPrograms(path: String): Unit = testFiles(path, testValidProgram)

  def testValidProgram(file: File): Unit = {
    val ctx = testContext(Some(file))

    try {
      val sources = FileSource(file) :: Nil
      val cus = Main.FrontEnd.execute(ctx)(sources)

      ctx.reporter.hasErrors shouldBe false

      Main.GenerateCode.execute(ctx)(cus)
      val res = programExecutor(ctx.outDirs.map(_.getAbsolutePath) + Constants.TDirectory, file)
      val resLines = lines(res)
      val sol = parseSolutions(file)
      assertCorrect(resLines, sol)
    } catch {
      case e: CompilationException  =>
        e.messages.print(MessageType.Error)
        fail("Compilation failed")
      case _: FileNotFoundException => fail(s"Invalid test, file not found: ${ file.getPath }")
    }
  }

  private def lines(str: String): List[String] = str.split("\\r?\\n").map(_.trim).toList

  private def parseSolutions(file: File): List[(Int, String)] =
    Source.getText(file).lines.zipWithIndex.collect {
      case (SolutionRegex(line), lineNumber) => (lineNumber + 1, line.trim)
    }.toList

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
