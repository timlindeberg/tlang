package tlang.compiler

import java.io.File

import tlang.compiler.error.CompilationException
import tlang.utils.Extensions._
import tlang.utils.FileSource

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.io.Source

/**
  * Created by Tim Lindeberg on 4/2/2016.
  */
trait ErrorTester extends Tester {

  import Tester._

  def testFile(file: File): Unit = {
    val ctx = Tester.getTestContext(Some(file))

    try {
      val sources = FileSource(file) :: Nil
      Pipeline.run(ctx)(sources)

      // If we got here there were no errors, check for warnings instaed
      if (!ctx.reporter.hasWarnings)
        fail("Test failed: No errors or warnings!")


      val warnings = ctx.reporter.warningMessage
      if (PrintErrors)
        print(warnings)

      val expectedWarnings = parseSolutions(file)
      val warningCodes = parseErrorCodes(warnings, ctx)
      assertCorrect(warningCodes, expectedWarnings, warnings)
    } catch {
      case t: CompilationException =>
        val errors = t.getMessage
        if (PrintErrors)
          print(errors)
        val errorCodes = parseErrorCodes(errors, ctx)
        val expectedErrors = parseSolutions(file)
        assertCorrect(errorCodes, expectedErrors, errors)
    }
  }

  private def parseSolutions(file: File): List[(Int, String)] = {
    val fileName = file.getPath
    using(Source.fromFile(fileName)) { source =>
      source.getLines().zipWithIndex.flatMap {
        case (SolutionRegex(line), lineNumber) =>
          line.split(",").map(res => (lineNumber + 1, res.trim))
        case _                                 => Nil
      }.toList
    }
  }

  private val ErrorCode   = """(?:Fatal|Warning|Error) ([A-Z]\d\d\d\d)""".r
  private val LineNumbers = """(\d+)""".r // First number in error message is the line number

  private def parseErrorCodes(errorMessages: String, ctx: Context): List[(Int, String)] = {
    val errors = errorMessages.clearAnsi.split(ctx.formatting.top).toList.drop(1)

    val lineNumbers = errors.map(LineNumbers.findFirstMatchIn(_).get.group(1).toInt)
    val errorCodes = errors.map(ErrorCode.findFirstMatchIn(_).get.group(1))
    lineNumbers.zip(errorCodes)
  }

  private def assertCorrect(results: List[(Int, String)], solutions: List[(Int, String)], errors: String): Unit = {
    def asString(l: List[(Int, String)]) = l map { case (lineNumber, msg) =>
      val num = s"$lineNumber:"
      f"$num%-4s $msg"
    }
    val resStrings = asString(results)
    val solStrings = asString(solutions)

    def extraInfo(i: Int) = formatTestFailedMessage(i + 1, resStrings, solStrings) + "\n" + errors

    val resMap = mutable.HashMap[Int, ArrayBuffer[String]]()
    results foreach { case (line, res) =>
      val l = resMap.getOrElse(line, ArrayBuffer[String]())
      l += res.trim
      resMap += line -> l
    }


    solutions.zipWithIndex foreach { case ((line, sol), i) =>
      resMap.get(line) match {
        case Some(res) =>
          val strim = sol.trim
          if (res.contains(strim))
            res -= strim
          else
            failTest(s"Expected $sol on line $line but found ${res.mkString(", ")}", extraInfo(i))
        case None      =>
          val errMsg = s"Line $line did not produce $sol"
          System.err.println(s"$errMsg ${extraInfo(i)}")
          fail(errMsg)
      }
    }
    resMap foreach { case (line, res) =>
      if (res.nonEmpty)
        failTest(s"Unexpected '${res.mkString(", ")}' was found on line $line", extraInfo(-1))
    }

  }

  private def failTest(msg: String, extraInfo: String): Nothing = {
    System.err.println(s"$msg $extraInfo")
    fail(msg)
  }

}
