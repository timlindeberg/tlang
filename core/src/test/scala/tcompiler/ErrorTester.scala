package tcompiler

import java.io.File

import tcompiler.error.CompilationException

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.io.Source

/**
  * Created by Tim Lindeberg on 4/2/2016.
  */
abstract class ErrorTester extends Tester {

  import TestUtils._

  private def Seperator = "---------------------------------------------------------------------\n"

  def testFile(file: File): Unit = {
    val ctx = getTestContext(file)
    val expectedErrors = parseSolutions(file)

    try {
      Pipeline.run(ctx)(List(file))
      // Check for warnings:
      if (ctx.reporter.warnings.isEmpty)
        fail("Test failed: No errors or warnings!")

      val warnings = ctx.reporter.warningsString
      if (PrintErrors)
        println(warnings)

      val warningCodes = parseErrorCodes(warnings)
      assertCorrect(warningCodes, expectedErrors, warnings)
    } catch {
      case t: CompilationException =>
        val errors = t.getMessage
        if (PrintErrors)
          println(errors)
        val errorCodes = parseErrorCodes(errors)
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

  private val AnsiRegex = """\x1b[^m]*m""".r
  private def removeANSIFormatting(s: String) = AnsiRegex.replaceAllIn(s, "")
  private val ErrorRegex = """.*\.kool:(\d+):.+?\n(?:Fatal|Warning|Error) \((.+?)\).*""".r
  // Parses codes from error messages

  private def parseErrorCodes(errorMessages: String): List[(Int, String)] = {
    // First two rows of error messages
    val errors = removeANSIFormatting(errorMessages).split("\n\n") map {
      _.split("\n").take(2).mkString("\n")
    }
    errors.collect {
      case ErrorRegex(lineNumber, errorCode) => (lineNumber.toInt, errorCode)
    }.toList
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
