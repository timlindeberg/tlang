package tlang.compiler

import better.files.File
import org.scalatest.ParallelTestExecution
import tlang.compiler.analyzer.{Flowing, Naming, Typing}
import tlang.compiler.ast.Parsing
import tlang.compiler.lexer.Lexing
import tlang.compiler.messages.{CompilationException, CompilerMessage, CompilerMessages, MessageType}
import tlang.compiler.modification.Templating
import tlang.testutils.TestConstants._
import tlang.utils.{FileSource, Source}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class CompilerErrorsSuite extends CompilerIntegrationTestSpec with ParallelTestExecution {


  val ErrorResources = s"$Resources/errortests"

  override def suiteName: String = "Compiler Errors"


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
        verifyMessages(file, ctx, e.messages, MessageType.Error)
        return
    }

    val reporter = ctx.reporter
    if (reporter.isEmpty)
      fail("Test failed: No errors or warnings!")

    verifyMessages(file, ctx, ctx.reporter.messages, MessageType.Warning)
  }

  private def verifyMessages(file: File, ctx: Context, messages: CompilerMessages, messageType: MessageType): Unit = {
    if (Verbose)
      printExecutionTimes(file, ctx)

    if (PrintErrors)
      ctx.messageFormatter.print(messages, messageType)

    val foundCodes = getErrorCodes(messages(messageType))
    val expectedCodes = parseSolutions(file)
    verifyResults(foundCodes, expectedCodes)
  }


  private def getErrorCodes(errors: List[CompilerMessage]) = errors.map { error => (error.pos.line, error.code) }

  private def parseSolutions(file: File): List[(Int, String)] =
    Source.getText(file).lines.zipWithIndex.flatMap {
      case (SolutionRegex(line), lineNumber) => line.split(",").map(res => (lineNumber + 1, res.trim))
      case _                                 => Nil
    }.toList


  private def verifyResults(results: List[(Int, String)], solutions: List[(Int, String)]): Unit = {
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
