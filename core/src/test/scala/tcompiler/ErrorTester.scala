package tcompiler

import java.io.File

import tcompiler.error.CompilationException
import tcompiler.utils.Context
import tcompiler.utils.Extensions._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.io.Source

/**
  * Created by Tim Lindeberg on 4/2/2016.
  */
trait ErrorTester extends Tester {

  import Tester._

  def testFile(file: File): Unit = {
    val ctx = Tester.getTestContext(file)

    try {
      Pipeline.run(ctx)(Set(file))
      // Check for warnings:
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
    Source.fromFile(fileName).getLines().zipWithIndex.flatMap {
      case (SolutionRegex(line), lineNumber) =>
        line.split(",").map(res => (lineNumber + 1, res.trim))
      case _                                 => Nil
    }.toList
  }

  private val ErrorCode   = """(?:Fatal|Warning|Error) ([A-Z]\d\d\d\d)""".r
  private val LineNumbers = """(\d+)""".r // First number in error message is the line number

  private def parseErrorCodes(errorMessages: String, ctx: Context): List[(Int, String)] = {
    import ctx.formatting._
    val errors = errorMessages.clearAnsi.split(top).toList.drop(2)
    val lineNumbers = errors.map(LineNumbers.findFirstMatchIn(_).get.group(1).toInt)
    val errorCodes = errors.map(ErrorCode.findFirstMatchIn(_).get.group(1))
    lineNumbers.zip(errorCodes)
  }

  private def assertCorrect(res: List[(Int, String)], sol: List[(Int, String)], errors: String): Unit = {
    def asString(l: List[(Int, String)]) = l map { case (lineNumber, msg) =>
      val num = s"$lineNumber:"
      f"$num%-4s $msg"
    }
    val resStrings = asString(res)
    val solStrings = asString(sol)


    val resMap = mutable.HashMap[Int, ArrayBuffer[String]]()
    res foreach { case (line, r) =>
      val l = resMap.getOrElse(line, ArrayBuffer[String]())
      l += r.trim
      resMap += line -> l
    }

    sol.zipWithIndex foreach { case ((line, s), i) =>
      val extraInfo = formatTestFailedMessage(i + 1, resStrings, solStrings, errors)
      resMap.get(line) match {
        case Some(r) =>
          val strim = s.trim
          if (r.contains(strim))
            r -= strim
          else
            failTest(s"Expected $s on line $line but found ${r.mkString(", ")}", extraInfo)
        case None    =>
          val errMsg = s"Line $line did not produce $s"
          System.err.println(s"$errMsg $extraInfo")
          fail(errMsg)
      }
    }
    val extraInfo = formatTestFailedMessage(-1, resStrings, solStrings, errors)
    resMap foreach { case (line, res) =>
      if (res.nonEmpty)
        failTest(s"Unexpected '${res.mkString(", ")}' was found on line $line", extraInfo)
    }

  }

  private def failTest(msg: String, extraInfo: String): Nothing = {
    System.err.println(s"$msg $extraInfo")
    fail(msg)
  }

}
