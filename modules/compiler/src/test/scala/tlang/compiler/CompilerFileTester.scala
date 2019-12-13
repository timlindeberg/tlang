package tlang
package compiler

import better.files.File
import tlang.compiler.analyzer.Symbols.Symbolic
import tlang.compiler.analyzer.Types.Typed
import tlang.compiler.argument.VerboseFlag
import tlang.compiler.ast.TreePrinter
import tlang.compiler.ast.Trees.{CompilationUnit, Tree}
import tlang.compiler.execution.Compiler
import tlang.compiler.messages.{CompilationException, CompilerMessages, MessageType}
import tlang.compiler.output.ErrorMessageOutput
import tlang.formatting.grid.{Column, TruncatedColumn}
import tlang.formatting.textformatters.SyntaxHighlighter
import tlang.utils._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.runtime.ScalaRunTime
import scala.util.matching.Regex

case class CompilerFileTester(
  file: File,
  ctx: Context,
  pipeline: CompilerPhase[Source, _]
)(
  implicit val syntaxHighlighter: SyntaxHighlighter) {

  case class TestResult(success: Boolean, message: String)

  import ctx.{formatter, options}

  private val SolutionRegex: Regex = """.*// *[R|r]es:(.*)""".r
  private val ErrorCodeRegex: Regex = """[A-Z]\d{4}.*""".r

  def execute(): TestResult = {
    try {
      executeTest(file)
    } catch {
      case e: TestFailedException => return TestResult(success = false, e.reason)
    }
    TestResult(success = true, "Test completed successfully")
  }

  private def executeTest(file: File): Unit = {
    val result = try {
      pipeline.execute(ctx)(FileSource(file) :: Nil)
    } catch {
      case e: CompilationException =>
        handleCompilationException(e.messages)
        return
    }

    val solutions = parseSolutions(file)
    val expectedCodes = solutions.filterInstance[ErrorMessageSolution]
    if (ctx.reporter.hasWarnings && expectedCodes.nonEmpty) {
      verifyErrorCodes(ctx.reporter.messages, MessageType.Warning, expectedCodes)
      return
    }

    if (ctx.reporter.hasErrors)
      fail("Compilation failed")

    val cus = getCUs(result)
    cus foreach verifyTypesAndSymbols

    Compiler.GenerateCode.execute(ctx)(cus)

    val output = executeProgram()
    verifyOutput(solutions, output)
  }

  private def handleCompilationException(messages: CompilerMessages): Unit = {
    val expectedCodes = parseSolutions(file).filterInstance[ErrorMessageSolution]
    if (expectedCodes.isEmpty) {
      ctx.output += ErrorMessageOutput(messages)
      fail("Compilation failed")
    }

    if (options(VerboseFlag)) {
      ctx.output += ErrorMessageOutput(messages, messageTypes = List(MessageType.Error))
    }

    verifyErrorCodes(messages, MessageType.Error, expectedCodes)
  }

  private def executeProgram(): ExecutionResult = {
    try {
      val mainMethodExecutor = DefaultMainMethodExecutor(ctx.allClassPaths)
      mainMethodExecutor(file)
    } catch {
      case _: NoSuchMethodException => fail("Expected output but test file did not contain a main method")
    }
  }

  private def verifyOutput(solutions: List[Solution], output: ExecutionResult): Unit = {
    output.exception.ifDefined { e => fail(s"Program execution failed with exception: ${ e.stackTrace }") }

    val expectedCodes = solutions.filterInstance[ErrorMessageSolution]
    if (expectedCodes.nonEmpty) {
      val codes = expectedCodes
        .map { case ErrorMessageSolution(line, errorCode) => s"$line: $errorCode" }
        .mkString(NL)
      fail("File compiled successfully but expected the following error codes:\n" + codes)
    }

    val resLines = lines(output.output)
    verifyOutputLines(resLines, solutions.filterInstance[OutputSolution])
  }

  private def verifyErrorCodes(messages: CompilerMessages, messageType: MessageType, solutions: List[ErrorMessageSolution]): Unit = {
    val foundCodes = messages(messageType).map { msg => (msg.pos.line, msg.code) }
      .map { case (line, code) => ErrorMessageSolution(line, code) }
    verifyErrorCodes(foundCodes, solutions)
  }

  private def verifyErrorCodes(results: List[ErrorMessageSolution], solutions: List[ErrorMessageSolution]): Unit = {
    def extraInfo(i: Int) = formatTestFailedMessage(i + 1, results, solutions) + NL

    val resultMap = mutable.HashMap[Int, ArrayBuffer[String]]()
    results foreach { case ErrorMessageSolution(line, res) =>
      val errorCode = resultMap.getOrElse(line, ArrayBuffer[String]())
      errorCode += res
      resultMap += line -> errorCode
    }

    solutions.zipWithIndex foreach { case (ErrorMessageSolution(line, sol), i) =>
      resultMap.get(line) match {
        case Some(res) =>
          val solTrimmed = sol.trim
          if (!res.contains(solTrimmed))
            fail(s"Expected $sol on line $line but found ${ res.mkString(", ") }\n" + extraInfo(i))
          res -= solTrimmed
        case None      =>
          fail(s"Line $line did not produce $sol\n" + extraInfo(i))
      }
    }

    resultMap foreach { case (line, res) =>
      if (res.nonEmpty)
        fail(s"Unexpected '${ res.mkString(", ") }' was found on line $line\n" + extraInfo(-1))
    }
  }

  private def getCUs(result: List[_]): List[CompilationUnit] = {
    if (result.isEmpty || !result.head.isInstanceOf[CompilationUnit]) {
      fail("Compilation succeeded but the result was not a compilation unit")
    }

    result.asInstanceOf[List[CompilationUnit]]
  }

  private def verifyTypesAndSymbols(cu: CompilationUnit): Unit = {
    def missing(t: Tree, missing: String) = {
      val treePrinter = new TreePrinter
      val treeRepr = ScalaRunTime._toString(t)

      ctx.formatter.grid
        .header(s"Tree $treeRepr has a missing $missing")
        .row(Column, TruncatedColumn, Column, Column, TruncatedColumn)
        .columnHeaders("Line", "Tree", "Reference", "Symbol", "Type")
        .contents(treePrinter(cu))
        .print()

      fail(s"Tree $treeRepr does not have a $missing.")
    }

    cu foreach { tree: Tree =>
      tree match {
        case s: Symbolic[_] if !s.hasSymbol => missing(tree, "symbol")
        case _                              =>
      }
      tree match {
        case t: Typed if !t.hasType => missing(tree, "type")
        case _                      =>
      }
    }
  }

  private def verifyOutputLines(results: List[OutputSolution], solutions: List[OutputSolution]): Unit = {
    def extraInfo(i: Int) = NL + formatTestFailedMessage(i + 1, results, solutions)

    results
      .zip(solutions)
      .zipWithIndex
      .foreach {
        case ((res, sol), i) if res.output != sol.output =>
          fail(s"Expected '${ sol.output }' but found '${ res.output }' at line ${ sol.line } ${ extraInfo(i) }")
        case _                                           =>
      }
    if (results.lengthCompare(solutions.length) != 0) {
      fail(s"Expected ${ solutions.length } lines but ${ results.length } were output ${ extraInfo(-1) }")
    }
  }

  private def formatTestFailedMessage(failedTest: Int, result: List[Solution], solution: List[Solution]): String = {
    val smallerFormatter = formatter.copy(lineWidth = formatter.lineWidth - 4)
    import smallerFormatter._

    def format(solution: Option[Solution]): String = solution match {
      case Some(Solution(lineNumber, output)) =>
        if (lineNumber == -1)
          return output
        val num = s"$lineNumber:"
        f"$num%-4s $output"
      case None                               => ""
    }

    val numbers = (1 to Math.max(result.length, solution.length)).map { i =>
      if (i == failedTest) s"$i" + " " + LeftArrow else s"$i"
    }

    val content = result.map(Some(_))
      .zipAll(solution.map(Some(_)), None, None)
      .zip(numbers)
      .map { case ((res, solution), num) => (num, format(res), format(solution)) }

    grid
      .row(3)
      .content("Num", "Result", "Solution")
      .contents(content)
      .render()
  }

  private def lines(str: String): List[OutputSolution] = {
    str
      .split("\\r?\\n")
      .map { line => OutputSolution(-1, line.trim) }
      .toList
  }

  private def parseSolutions(file: File): List[Solution] =
    FileSource
      .getText(file)
      .lines
      .zipWithIndex
      .flatMap {
        case (SolutionRegex(line), lineNumber) => parseLine(lineNumber + 1, line.trim)
        case _                                 => Nil
      }
      .toList

  private def parseLine(lineNumber: Int, line: String): List[Solution] = {
    if (ErrorCodeRegex.matches(line))
      line.split(",").map(res => ErrorMessageSolution(lineNumber, res.trim)).toList
    else
      OutputSolution(lineNumber, line) :: Nil
  }

  private def fail(reason: String): Nothing = throw TestFailedException(reason)

  private case class TestFailedException(reason: String) extends RuntimeException

  private trait Solution {
    def line: Int
    val output: String
  }

  object Solution {
    def unapply(arg: Solution): Option[(Int, String)] = Some((arg.line, arg.output))
  }

  private case class ErrorMessageSolution(override val line: Int, errorCode: String) extends Solution {
    val output: String = errorCode
  }
  private case class OutputSolution(override val line: Int, override val output: String) extends Solution

}
