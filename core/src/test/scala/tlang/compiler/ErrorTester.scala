package tlang.compiler

import java.io.File

import tlang.compiler.error.{CompilationException, ErrorMessage}
import tlang.utils.{FileSource, Source}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

trait ErrorTester extends Tester {

  import Tester._

  def testFile(file: File): Unit = {
    val ctx = Tester.getTestContext(Some(file))
    val sources = FileSource(file) :: Nil

    try {
      Pipeline.execute(ctx)(sources)
    } catch {
      case e: CompilationException =>
        if (PrintErrors)
          e.messages.printErrors()
        val errorCodes = getErrorCodes(e.messages.getErrors)
        val expectedErrors = parseSolutions(file)
        assertCorrect(errorCodes, expectedErrors)
        return
    }

    // If we got here there were no errors, check for warnings instaed
    val reporter = ctx.reporter
    if (!reporter.hasWarnings)
      fail("Test failed: No errors or warnings!")

    if (PrintErrors)
      reporter.messages.printWarnings()

    val expectedWarnings = parseSolutions(file)
    val warningCodes = getErrorCodes(reporter.messages.getWarnings)
    assertCorrect(warningCodes, expectedWarnings)
  }

  private def getErrorCodes(errors: List[ErrorMessage]) = errors.map { error => (error.pos.line, error.code) }

  private def parseSolutions(file: File): List[(Int, String)] =
    Source.getText(file).lines.zipWithIndex.flatMap {
      case (SolutionRegex(line), lineNumber) => line.split(",").map(res => (lineNumber + 1, res.trim))
      case _                                 => Nil
    }.toList


  private def assertCorrect(results: List[(Int, String)], solutions: List[(Int, String)]): Unit = {
    def asString(l: List[(Int, String)]) = l map { case (lineNumber, msg) =>
      val num = s"$lineNumber:"
      f"$num%-4s $msg"
    }
    val resStrings = asString(results)
    val solStrings = asString(solutions)

    def extraInfo(i: Int) = formatTestFailedMessage(i + 1, resStrings, solStrings) + "\n"

    val resMap = mutable.HashMap[Int, ArrayBuffer[String]]()
    results foreach { case (line, res) =>
      val l = resMap.getOrElse(line, ArrayBuffer[String]())
      l += res.trim
      resMap += line -> l
    }

    solutions.zipWithIndex foreach { case ((line, sol), i) =>
      resMap.get(line) match {
        case Some(res) =>
          val solTrimmed = sol.trim
          if (!res.contains(solTrimmed))
            failTest(s"Expected $sol on line $line but found ${ res.mkString(", ") }", extraInfo(i))
          res -= solTrimmed
        case None      =>
          failTest(s"Line $line did not produce $sol", extraInfo(i))
      }
    }

    resMap foreach { case (line, res) =>
      if (res.nonEmpty)
        failTest(s"Unexpected '${ res.mkString(", ") }' was found on line $line", extraInfo(-1))
    }

  }

  private def failTest(msg: String, extraInfo: String): Nothing = {
    System.err.println(s"$msg $extraInfo")
    fail(msg)
  }

}
