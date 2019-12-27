package tlang.testutils

import better.files.File
import tlang.NL
import tlang.compiler.analyzer.Symbols.Symbolic
import tlang.compiler.analyzer.Types
import tlang.compiler.analyzer.Types.Typed
import tlang.compiler.argument.VerboseFlag
import tlang.compiler.ast.Trees._
import tlang.compiler.ast.{TreePrinter, Trees}
import tlang.compiler.execution.Compiler
import tlang.compiler.messages.{CompilationException, CompilerMessages, MessageType}
import tlang.compiler.output.{ErrorMessageOutput, SimpleOutput}
import tlang.compiler.testutils.{ErrorMessageSolution, OutputSolution, Solution}
import tlang.compiler.{CompilerPhase, Context}
import tlang.formatting.Colors
import tlang.formatting.grid.{Column, TruncatedColumn}
import tlang.formatting.textformatters.{StackTraceHighlighter, SyntaxHighlighter}
import tlang.options.argument.MessageContextFlag
import tlang.testutils.solutions.{ErrorMessageSolution, OutputSolution, Solution, SolutionParser}
import tlang.utils._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.runtime.ScalaRunTime

case class TestResult(success: Boolean, reason: String, extraInfo: List[String])

case class CompilerFileTester(file: File, ctx: Context, pipeline: CompilerPhase[Source, _])
  (implicit val syntaxHighlighter: SyntaxHighlighter, stackTraceHighlighter: StackTraceHighlighter) {

  import ctx.{formatter, options}

  private val solutionParser = SolutionParser(ctx)

  def execute(): TestResult = {
    try {
      executeTest(file)
    } catch {
      case e: TestFailedException => return TestResult(success = false, e.reason, e.extraBoxes)
    }
    TestResult(success = true, "Test completed successfully", Nil)
  }

  private def executeTest(file: File): Unit = {
    val result = try {
      pipeline.execute(ctx)(FileSource(file) :: Nil)
    } catch {
      case e: CompilationException =>
        handleCompilationException(e.messages)
        return
    }

    val solutions = solutionParser.parse(file)
    val expectedCodes = solutions.filterInstance[ErrorMessageSolution]
    if (ctx.reporter.hasWarnings && expectedCodes.nonEmpty) {
      verifyErrorCodes(ctx.reporter.messages, MessageType.Warning, expectedCodes)
      return
    }

    if (ctx.reporter.hasErrors)
      fail("Compilation failed")

    if (expectedCodes.nonEmpty) {
      val codes = expectedCodes
        .map { case ErrorMessageSolution(line, errorCode) => s"$line: $errorCode" }
        .mkString(NL)
      fail("File compiled successfully but expected the following error codes:", codes)
    }

    val cus = getCUs(result)
    cus foreach verifyTypesAndSymbols

    val withLinePrinting = cus.map { appendLineToPrintStatements }
    Compiler.GenerateCode.execute(ctx)(withLinePrinting)

    val expectedOutput = solutions.filterInstance[OutputSolution]
    if (expectedOutput.isEmpty) {
      return
    }

    val output = executeProgram()
    verifyOutput(solutions, output)
  }

  private def appendLineToPrintStatements(compilationUnit: CompilationUnit) = {
    def appendLinePrefix(pos: Positioned, expr: ExprTree) = {
      val linePrefix = StringLit(s"${ pos.lineEnd }: ").setPos(expr)
      Plus(linePrefix, expr).setPos(expr).setType(Types.String)
    }

    val transformer = new Trees.Transformer {
      def transformation: TreeTransformation = {
        case p@Println(expr) => copier.Println(p, appendLinePrefix(p, expr))
        case p@Print(expr)   => copier.Print(p, appendLinePrefix(p, expr))
      }
    }
    transformer(compilationUnit)
  }

  private def handleCompilationException(messages: CompilerMessages): Unit = {
    val messageContext = ctx.options(MessageContextFlag)
    val expectedCodes = solutionParser.parse(file).filterInstance[ErrorMessageSolution]
    if (expectedCodes.isEmpty) {
      val errors = ErrorMessageOutput(messages, messageContext).pretty
      fail("Compilation failed:", errors)
    }

    if (options(VerboseFlag)) {
      ctx.output += ErrorMessageOutput(messages, messageContext, List(MessageType.Error))
    }

    verifyErrorCodes(messages, MessageType.Error, expectedCodes)
  }

  private def executeProgram(): ExecutionResult = {
    try {
      val mainMethodExecutor = DefaultMainMethodExecutor(ctx.allClassPaths)
      mainMethodExecutor(file)
    } catch {
      case _: NoSuchMethodException | _: ClassNotFoundException =>
        fail("Expected output but test file did not produce any output")
    }
  }

  private def verifyOutput(solutions: IndexedSeq[Solution], result: ExecutionResult): Unit = {
    result.exception.ifDefined { e =>
      val outputInfo = SimpleOutput("Output before exception:\n" + result.output).pretty
      val stackTraceInfo = SimpleOutput(stackTraceHighlighter(e)).pretty
      fail(s"Program execution failed with exception: ${ e.getClass.getName } ", outputInfo, stackTraceInfo)
    }
    val resLines = solutionParser.parse(result)
    verifyOutputLines(resLines, solutions.filterInstance[OutputSolution])
  }

  private def verifyErrorCodes(messages: CompilerMessages, messageType: MessageType, solutions: IndexedSeq[ErrorMessageSolution]): Unit = {
    val foundCodes = messages(messageType)
      .sortBy { _.pos.line }
      .map { msg => (msg.pos.line, msg.code) }
      .map { case (line, code) => ErrorMessageSolution(line, code) }
      .toIndexedSeq
    verifyErrorCodes(foundCodes, solutions)
  }

  private def verifyErrorCodes(results: IndexedSeq[ErrorMessageSolution], solutions: IndexedSeq[ErrorMessageSolution]): Unit = {
    def extraInfo = formatTestDifference(results, solutions, colorSolution = true)

    val resultMap = mutable.HashMap[Int, ArrayBuffer[String]]()
    results foreach { case ErrorMessageSolution(line, res) =>
      val errorCode = resultMap.getOrElse(line, ArrayBuffer[String]())
      errorCode += res
      resultMap += line -> errorCode
    }

    solutions foreach { case ErrorMessageSolution(line, sol) =>
      resultMap.get(line) match {
        case Some(res) =>
          val solTrimmed = sol.trim
          if (!res.contains(solTrimmed))
            fail(s"Expected $sol on line $line but found ${ res.mkString(", ") }", extraInfo)
          res -= solTrimmed
        case None      =>
          fail(s"Line $line did not produce $sol", extraInfo)
      }
    }

    resultMap foreach { case (line, res) =>
      if (res.nonEmpty)
        fail(s"Unexpected '${ res.mkString(", ") }' was found on line $line", extraInfo)
    }
  }

  private def getCUs(result: List[_]): List[CompilationUnit] = {
    if (result.isEmpty || !result.head.isInstanceOf[CompilationUnit]) {
      fail("Compilation succeeded but the result was not a compilation unit")
    }

    result.asInstanceOf[List[CompilationUnit]]
  }

  private def verifyTypesAndSymbols(cu: CompilationUnit): Unit = {
    def failMissing(t: Tree, missing: String) = {
      val treePrinter = new TreePrinter
      val treeRepr = ScalaRunTime._toString(t)

      val debugTree = formatter.grid
        .header(s"Tree $treeRepr has a missing $missing")
        .row(Column, TruncatedColumn, Column, Column, TruncatedColumn)
        .columnHeaders("Line", "Tree", "Reference", "Symbol", "Type")
        .contents(treePrinter(cu))
        .render()

      fail(s"Tree $treeRepr does not have a $missing:", debugTree)
    }

    cu foreach { tree: Tree =>
      tree match {
        case s: Symbolic[_] if !s.hasSymbol => failMissing(tree, "symbol")
        case _                              =>
      }
      tree match {
        case t: Typed if !t.hasType => failMissing(tree, "type")
        case _                      =>
      }
    }
  }

  private def verifyOutputLines(results: IndexedSeq[Solution], solutions: IndexedSeq[Solution]): Unit = {
    def extraInfo = formatTestDifference(results, solutions, colorSolution = false)

    results
      .zip(solutions)
      .zipWithIndex
      .foreach {
        case ((res, sol), i) if res.content != sol.content =>
          fail(s"Expected '${ sol.content }' but found '${ res.content }' at line ${ sol.line }", extraInfo)
        case _                                             =>
      }
    if (results.lengthCompare(solutions.length) != 0) {
      fail(s"Expected ${ solutions.length } lines but ${ results.length } were output", extraInfo)
    }
  }

  private def formatTestDifference(results: IndexedSeq[Solution], solutions: IndexedSeq[Solution], colorSolution: Boolean): String = {
    val maxLineWidth = (results ++ solutions).map { _.line.digits }.max

    def format(solution: Solution, isMatch: Boolean): String = {
      import formatter._
      val Solution(lineNumber, output) = solution
      val outputColor = if (colorSolution) getColor(output) else NoColor
      val lineColor = Bold + (if (isMatch) Green else Red)
      val padded = lineNumber.toString.padTo(maxLineWidth, ' ')
      lineColor(padded) + ": " + outputColor(output)
    }

    val grid = formatter.grid
      .row(2)
      .columnHeaders("Result", "Solution")

    var resIndex = 0
    var solIndex = 0
    while (resIndex < results.size && solIndex < solutions.size) {
      val result = results(resIndex)
      val solution = solutions(solIndex)
      if (result.line < solution.line) {
        resIndex += 1
        grid.content(format(result, isMatch = false), "")
      } else if (solution.line < result.line) {
        solIndex += 1
        grid.content("", format(result, isMatch = false))
      } else {
        resIndex += 1
        solIndex += 1
        val isMatch = result.content == solution.content
        grid.content(format(result, isMatch), format(solution, isMatch))
      }
    }

    grid.render()
  }

  private def getColor(s: String): Colors.Color = getColor(s.hashCode)

  private def getColor(i: Int): Colors.Color = {
    val colors = formatter.FGColors
    colors(i % colors.length)
  }

  private def fail(reason: String, extraBoxes: String*): Nothing = {
    throw TestFailedException(reason, extraBoxes.toList)
  }

  private case class TestFailedException(reason: String, extraBoxes: List[String]) extends RuntimeException

}
