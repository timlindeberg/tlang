package tlang.compiler

import better.files.File
import org.scalatest.ParallelTestExecution
import tlang.compiler.analyzer.{Flowing, Naming, Typing}
import tlang.compiler.ast.Parsing
import tlang.compiler.lexer.Lexing
import tlang.compiler.modification.Templating
import tlang.messages.{CompilationException, CompilerMessage, MessageType}
import tlang.testsuites.CompilerIntegrationTests
import tlang.utils.{FileSource, Source}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class CompilerErrors extends CompilerIntegrationTestSpec with ParallelTestExecution {

  import CompilerIntegrationTests._

  val ErrorResources = s"$Resources/errortests"


  testFileForErrors(s"$ErrorResources/Lexing", Lexing)
  testFileForErrors(s"$ErrorResources/Parsing", Lexing andThen Parsing)
  testFileForErrors(s"$ErrorResources/Templating", Lexing andThen Parsing andThen Templating)
  testFileForErrors(s"$ErrorResources/Naming", Lexing andThen Parsing andThen Templating andThen Naming)
  testFileForErrors(s"$ErrorResources/Typing", Lexing andThen Parsing andThen Templating andThen Naming andThen Typing)
  testFileForErrors(s"$ErrorResources/Flowing", Lexing andThen Parsing andThen Templating andThen Naming andThen Typing andThen Flowing)


  private def testFileForErrors[T](path: String, pipeLine: CompilerPhase[Source, T]): Unit = testFiles(path, testFileForErrors(pipeLine, _))

  private def testFileForErrors[T](pipeLine: CompilerPhase[Source, T], file: File): Unit = {
    val ctx = testContext(Some(file))
    val sources = FileSource(file) :: Nil

    try {
      pipeLine.execute(ctx)(sources)
    } catch {
      case e: CompilationException =>
        if (PrintErrors)
          e.messages.print(MessageType.Error)
        val errorCodes = getErrorCodes(e.messages(MessageType.Error))
        val expectedErrors = parseSolutions(file)
        assertResultsEqualsSolutions(errorCodes, expectedErrors)
        return
    }

    // If we got here there were no errors, check for warnings instead
    val reporter = ctx.reporter
    if (!reporter.hasWarnings)
      fail("Test failed: No errors or warnings!")

    if (PrintErrors)
      reporter.printWarnings()

    val expectedWarnings = parseSolutions(file)
    val warningCodes = getErrorCodes(reporter.getWarnings)
    assertResultsEqualsSolutions(warningCodes, expectedWarnings)
  }


  private def getErrorCodes(errors: List[CompilerMessage]) = errors.map { error => (error.pos.line, error.code) }

  private def parseSolutions(file: File): List[(Int, String)] =
    Source.getText(file).lines.zipWithIndex.flatMap {
      case (SolutionRegex(line), lineNumber) => line.split(",").map(res => (lineNumber + 1, res.trim))
      case _                                 => Nil
    }.toList


  private def assertResultsEqualsSolutions(results: List[(Int, String)], solutions: List[(Int, String)]): Unit = {
    def asString(l: List[(Int, String)]) = l map { case (lineNumber, msg) =>
      val num = s"$lineNumber:"
      f"$num%-4s $msg"
    }
    val resStrings = asString(results)
    val solStrings = asString(solutions)

    def extraInfo(i: Int) = formatTestFailedMessage(i + 1, resStrings, solStrings) + "\n"

    val resultMap = mutable.HashMap[Int, ArrayBuffer[String]]()
    results foreach { case (line, res) =>
      val l = resultMap.getOrElse(line, ArrayBuffer[String]())
      l += res.trim
      resultMap += line -> l
    }

    solutions.zipWithIndex foreach { case ((line, sol), i) =>
      resultMap.get(line) match {
        case Some(res) =>
          val solTrimmed = sol.trim
          if (!res.contains(solTrimmed))
            failTest(s"Expected $sol on line $line but found ${ res.mkString(", ") }", extraInfo(i))
          res -= solTrimmed
        case None      =>
          failTest(s"Line $line did not produce $sol", extraInfo(i))
      }
    }

    resultMap foreach { case (line, res) =>
      if (res.nonEmpty)
        failTest(s"Unexpected '${ res.mkString(", ") }' was found on line $line", extraInfo(-1))
    }

  }

  private def failTest(msg: String, extraInfo: String): Nothing = {
    System.err.println(s"$msg\n$extraInfo")
    fail(msg)
  }

}
