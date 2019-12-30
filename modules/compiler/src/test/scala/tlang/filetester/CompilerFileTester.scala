package tlang
package filetester

import better.files.File
import tlang.compiler.analyzer.Symbols.Symbolic
import tlang.compiler.analyzer.Types
import tlang.compiler.analyzer.Types.Typed
import tlang.compiler.argument.VerboseFlag
import tlang.compiler.ast.Trees._
import tlang.compiler.ast.{TreePrinter, Trees}
import tlang.compiler.execution.Compiler
import tlang.compiler.lowering.TreeBuilder
import tlang.compiler.messages.{CompilationException, CompilerMessages, MessageType}
import tlang.compiler.output.debug.ASTOutput
import tlang.compiler.output.{ErrorMessageOutput, SimpleOutput}
import tlang.compiler.{CompilerPhase, Context}
import tlang.formatting.grid.{Column, TruncatedColumn}
import tlang.formatting.textformatters.{StackTraceHighlighter, SyntaxHighlighter}
import tlang.options.argument.MessageContextFlag
import tlang.utils._

import scala.runtime.ScalaRunTime

case class CompilerFileTester(file: File, ctx: Context, pipeline: CompilerPhase[Source, _])
  (implicit val syntaxHighlighter: SyntaxHighlighter, stackTraceHighlighter: StackTraceHighlighter) {

  import ctx.{formatter, options}

  private val solutionParser = SolutionParser(ctx)
  private val solutionVerifier = SolutionVerifier()
  private val treePrinter = TreePrinter()

  def execute(): TestResult = {
    try {
      executeTest(file)
    } catch {
      case e: TestFailedException => return TestResult(success = false, e.reason, e.extraBoxes)
    }
    TestResult(success = true, "Test completed successfully", Nil)
  }

  private def executeTest(file: File): Unit = {
    val SolutionParserResult(expectedCodes, expectedOutput) = solutionParser.parse(file)

    val result = try {
      pipeline.execute(ctx)(FileSource(file) :: Nil)
    } catch {
      case e: CompilationException =>
        handleCompilationException(e.messages, expectedCodes)
        return
    }

    if (ctx.reporter.hasWarnings && expectedCodes.nonEmpty) {
      verifyErrorCodes(ctx.reporter.messages, MessageType.Warning, expectedCodes)
      return
    }

    if (expectedCodes.nonEmpty) {
      solutionVerifier(IndexedSeq(), expectedCodes, colorContent = true)
      return
    }

    if (ctx.reporter.hasErrors)
      fail("Compilation failed")

    val cus = getCUs(result)
    cus foreach verifyTree

    val withLinePrinting = cus.map { appendLineNumberToPrintStatements }
    Compiler.GenerateCode.execute(ctx)(withLinePrinting)

    if (expectedOutput.nonEmpty) {
      val output = executeProgram()
      verifyOutput(expectedOutput, output)
    }
  }

  private def appendLineNumberToPrintStatements(compilationUnit: CompilationUnit) = {
    // Appends the line number to println statements. Example:
    //
    // 25: println(1 + 2 + 3)
    // ->
    // 25: println((String.ValueOf(1 + 2 + 3) + ":25")
    //
    // We call String.ValueOf to make sure we get the plus operator of String and
    // not some overloaded string operator. It also handles nulls and arrays
    def appendLineNumber(pos: Positioned, expr: ExprTree) = {
      val treeBuilder = new TreeBuilder
      val stringClass = ClassID(Types.StringSymbol.name).setSymbol(Types.StringSymbol).setPos(expr)
      val sym = Types.StringSymbol.lookupMethod("ValueOf", expr.getType :: Nil, compilationUnit.imports)
      val stringValueOfCall = treeBuilder.createMethodCall(stringClass, sym.get, expr)
      val linePostfix = StringLit(s":${ pos.lineEnd }").setPos(expr)
      Plus(stringValueOfCall, linePostfix).setPos(expr).setType(Types.String)
    }

    val transformer = new Trees.Transformer {
      def transformation: TreeTransformation = {
        case p@Println(expr) => copier.Println(p, appendLineNumber(p, expr))
      }
    }
    transformer(compilationUnit)
  }

  private def handleCompilationException(messages: CompilerMessages, expectedCodes: IndexedSeq[Solution]): Unit = {
    val messageContext = ctx.options(MessageContextFlag)
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
      import formatter._
      val stackTraceInfo = SimpleOutput(stackTraceHighlighter(e)).pretty
      val message = s"Program execution failed with exception: ${ Red(e.getClass.getName) }"
      if (result.output.nonEmpty) {
        val outputInfo = SimpleOutput(s"Output before exception:$NL" + result.output).pretty
        fail(message, outputInfo, stackTraceInfo)
      } else {
        fail(message, stackTraceInfo)
      }
    }

    val results = solutionParser.parse(result)
    solutionVerifier(results, solutions, colorContent = false)
  }

  private def verifyErrorCodes(messages: CompilerMessages, messageType: MessageType, solutions: IndexedSeq[Solution]): Unit = {
    val foundCodes = solutionParser.parse(messages(messageType))
    solutionVerifier(foundCodes, solutions, colorContent = true)
  }

  private def getCUs(result: List[_]): List[CompilationUnit] = {
    if (result.isEmpty || !result.head.isInstanceOf[CompilationUnit]) {
      fail("Compilation succeeded but the result was not a compilation unit")
    }

    result.asInstanceOf[List[CompilationUnit]]
  }

  private def verifyTree(cu: CompilationUnit): Unit = {
    def invalidTree(tree: Tree, missing: String) = {
      val treeRepr = ScalaRunTime._toString(tree)
      fail(s"Tree $treeRepr does not have a $missing:", treePrinter.drawGrid(tree))
    }

    cu foreach { tree: Tree =>
      tree match {
        case s: Symbolic[_] if !s.hasSymbol => invalidTree(tree, "symbol")
        case _                              =>
      }
      tree match {
        case t: Typed if !t.hasType => invalidTree(tree, "type")
        case _                      =>
      }
      if (tree.getPos == UninitializedPosition) {
        invalidTree(tree, "position")
      }
    }
  }

  private def fail(reason: String, extraBoxes: String*): Nothing = {
    throw TestFailedException(reason, extraBoxes.toList)
  }
}
