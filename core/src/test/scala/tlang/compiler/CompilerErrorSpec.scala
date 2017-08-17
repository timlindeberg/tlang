package tlang.compiler

import java.io.File

import org.scalatest.{FreeSpec, ParallelTestExecution}
import tlang.compiler.analyzer.{Flowing, Naming, Typing}
import tlang.compiler.ast.Parsing
import tlang.compiler.error.{CompilationException, CompilerMessage, MessageType}
import tlang.compiler.lexer.Lexing
import tlang.compiler.modification.Templating
import tlang.testutils.CompilerTestSpec
import tlang.utils.{FileSource, Source}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class CompilerErrorSpec extends FreeSpec with CompilerTestSpec with ParallelTestExecution {

  val ErrorResources = s"$Resources/errortests"


  "Lexing" - {
    testFileForErrors(s"$ErrorResources/lexing", Lexing)
  }

  "Parsing" - {
    testFileForErrors(s"$ErrorResources/parsing", Lexing andThen Parsing)
  }

  "Templating" - {
    testFileForErrors(s"$ErrorResources/templating", Lexing andThen Parsing andThen Templating)
  }

  "Naming" - {
    testFileForErrors(s"$ErrorResources/naming", Lexing andThen Parsing andThen Templating andThen Naming)
  }

  "Typing" - {
    testFileForErrors(s"$ErrorResources/typing", Lexing andThen Parsing andThen Templating andThen Naming andThen Typing)
  }

  "Flowing" - {
    testFileForErrors(s"$ErrorResources/flowing", Lexing andThen Parsing andThen Templating andThen Naming andThen Typing andThen Flowing)
  }


  private def testFileForErrors[T](path: String, pipeLine: CompilerPhase[Source, T]): Unit = {
    testFiles(path, testFileForErrors(pipeLine, _))
  }

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
        assertCorrect(errorCodes, expectedErrors)
        return
    }

    // If we got here there were no errors, check for warnings instaed
    val reporter = ctx.reporter
    if (!reporter.hasWarnings)
      fail("Test failed: No errors or warnings!")

    if (PrintErrors)
      reporter.printWarnings()

    val expectedWarnings = parseSolutions(file)
    val warningCodes = getErrorCodes(reporter.getWarnings)
    assertCorrect(warningCodes, expectedWarnings)
  }


  private def getErrorCodes(errors: List[CompilerMessage]) = errors.map { error => (error.pos.line, error.code) }

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
