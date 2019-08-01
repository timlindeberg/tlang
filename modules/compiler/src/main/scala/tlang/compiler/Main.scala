package tlang
package compiler

import cafebabe.CodegenerationStackTrace
import tlang.Constants._
import tlang.compiler.analyzer.{Flowing, Naming, Typing}
import tlang.compiler.argument._
import tlang.compiler.ast.Parsing
import tlang.compiler.ast.Trees._
import tlang.compiler.code.{CodeGeneration, Lowering}
import tlang.compiler.imports.ClassPath
import tlang.compiler.lexer.Lexing
import tlang.compiler.messages._
import tlang.compiler.modification.Templating
import tlang.compiler.output._
import tlang.compiler.output.help.{FlagInfoOutput, HelpOutput, PhaseInfoOutput, VersionOutput}
import tlang.compiler.utils.TLangSyntaxHighlighter
import tlang.formatting.grid.Alignment.Center
import tlang.formatting.grid.{Column, OverflowHandling}
import tlang.formatting.grid.Width.Fixed
import tlang.formatting.textformatters.{StackTraceHighlighter, SyntaxHighlighter}
import tlang.formatting.{ErrorStringContext, Formatter}
import tlang.options.argument._
import tlang.options.{FlagArgument, Options}
import tlang.utils._

case class ExitException(code: Int) extends Throwable

object Main extends Logging {

  val FrontEnd: CompilerPhase[Source, CompilationUnit] =
    Lexing andThen
      Parsing andThen
      Templating andThen
      Naming andThen
      Typing andThen
      Flowing

  val GenerateCode: CompilerPhase[CompilationUnit, CodegenerationStackTrace] =
    Lowering andThen CodeGeneration

  val Compiler: CompilerPhase[Source, CodegenerationStackTrace] = FrontEnd andThen GenerateCode

  val CompilerPhases: Seq[CompilerPhase[_, _]] = List(
    Lexing,
    Parsing,
    Templating,
    Naming,
    Typing,
    Flowing,
    Lowering,
    CodeGeneration
  )

  val CompilerFlags: Set[FlagArgument[_]] = Set(
    AsciiFlag,
    ClassPathFlag,
    ColorSchemeFlag,
    CompilerHelpFlag,
    DirectoryFlag,
    ExecFlag,
    IgnoredDefaultImportsFlag,
    JSONFlag,
    LineWidthFlag,
    LogLevelFlag,
    MaxErrorsFlag,
    MessageContextFlag,
    NoColorFlag,
    PrintOutputFlag,
    ReadStdinFlag,
    SuppressWarningsFlag,
    TabWidthFlag,
    ThreadsFlag,
    VerboseFlag,
    VersionFlag,
    WarningIsErrorFlag,
    WatchFlag
  )

  def main(args: Array[String]): Unit = {
    val options = parseOptions(args)
    implicit val formatter: Formatter = Formatter(
      lineWidth = options(LineWidthFlag),
      tabWidth = options(TabWidthFlag),
      colorScheme = options(ColorSchemeFlag),
      useColor = !(options(NoColorFlag) || options(JSONFlag)),
      asciiOnly = options(AsciiFlag)
    )

    Logging.DefaultLogSettings.formatter = formatter
    Logging.DefaultLogSettings.logLevel = options(LogLevelFlag)
    Logging.DefaultLogSettings.logThreads = options(ThreadsFlag).isInstanceOf[ParallellExecutor]

    val ctx = createContext(options)
    Main(ctx).run()
  }

  private def parseOptions(args: Array[String]): Options = {
    val errorContext = ErrorStringContext()(Formatter.SimpleFormatter)
    try {
      Options(flags = CompilerFlags, positionalArgument = Some(TFilesArgument), arguments = args)(errorContext)
    } catch {
      case e: IllegalArgumentException =>
        println(e.getMessage)
        sys.exit(1)
    }
  }

  private def createContext(options: Options)(implicit formatter: Formatter): Context = {
    info"Creating context with options $options"

    val messages = CompilerMessages(
      maxErrors = options(MaxErrorsFlag),
      warningIsError = options(WarningIsErrorFlag),
      suppressWarnings = options(SuppressWarningsFlag)
    )
    val outputHandler = if (options(JSONFlag)) JSONOutputHandler() else PrettyOutputHandler()
    Context(
      reporter = DefaultReporter(messages = messages),
      outputHandler = outputHandler,
      classPath = ClassPath.Default ++ options(ClassPathFlag),
      options = options
    )
  }
}


case class Main(ctx: Context) extends Logging {

  import Main._
  import ctx._

  private implicit val syntaxHighlighter    : SyntaxHighlighter     = TLangSyntaxHighlighter()
  private implicit val stackTraceHighlighter: StackTraceHighlighter = StackTraceHighlighter(failOnError = false)

  def run(): Unit = {
    sys.addShutdownHook {
      ctx.output += InterruptedOutput()
      exit(0)
    }

    if (options.isEmpty) {
      ctx.output += HelpOutput(CompilerFlags)
      exit(1)
    }

    printHelp()

    if (!isValidTHomeDirectory(THomeDirectory))
      ErrorInvalidTHomeDirectory(THomeDirectory)

    val sources = getSources
    if (sources.isEmpty)
      ErrorNoSourcesGiven()

    info"Compiling ${ sources.size } sources: ${ sources.map { _.description }.mkString(NL) }"

    if (options(VerboseFlag))
      ctx.output += SourcesOutput(sources)

    val CUs = compileAndExecute(sources)

    if (options(WatchFlag))
      CompilationWatcher(ctx, CUs, options, sources, compileAndExecute).watch()
    exit(0)
  }


  private def getSources: List[Source] = options(TFilesArgument).map(FileSource(_)).toList ++ sourceFromStdin

  private def sourceFromStdin(): List[Source] = {
    if (options(ReadStdinFlag)) List(StdinSource()) else List()
  }

  private def compileAndExecute(sources: List[Source]): Seq[CompilationUnit] = {
    try {
      val CUs = runCompiler(sources)
      printExecutionTimes(success = true)
      if (options(ExecFlag))
        executePrograms(CUs)
      CUs
    } catch {
      case ExitException(code) =>
        if (!options(WatchFlag))
          exit(code)
        Nil
    }
  }

  private def runCompiler(sources: List[Source]): Seq[CompilationUnit] = {
    try {
      val CUs = runFrontend(sources)
      GenerateCode.execute(ctx)(CUs)
      val messages = ctx.reporter.messages
      ctx.output += ErrorMessageOutput(messages, options(MessageContextFlag), List(MessageType.Warning))
      CUs
    } catch {
      case e: ExitException => throw e
      case e: Throwable     => internalError(e)
    }
  }

  private def runFrontend(sources: List[Source]): List[CompilationUnit] = {
    try {
      FrontEnd.execute(ctx)(sources)
    } catch {
      case e: CompilationException =>
        ctx.output += ErrorMessageOutput(e.messages, options(MessageContextFlag))
        printExecutionTimes(success = false)
        tryExit(1)
    }
  }

  // Top level error handling for any unknown exception
  private def internalError(error: Throwable): Nothing = {
    error"Execution error occurred: ${ error.stackTrace }"

    ctx.output += InternalErrorOutput(error)
    printExecutionTimes(success = false)
    tryExit(1)
  }

  private def printHelp(): Unit = {
    if (options(VersionFlag)) {
      ctx.output += VersionOutput()
      exit(0)
    }
    val args = options(CompilerHelpFlag)

    if (HelpFlag.DefaultArg in args) {
      ctx.output += HelpOutput(CompilerFlags)
      exit(0)
    }

    if (CompilerHelpFlag.Phases in args) {
      ctx.output += PhaseInfoOutput(CompilerPhases)
    }

    args foreach { arg =>
      CompilerFlags.find(_.name == arg) ifDefined { ctx.output += FlagInfoOutput(_) }
    }

    if (args.nonEmpty)
      exit(0)
  }

  private def isValidTHomeDirectory(path: String): Boolean = {
    // TODO: Make this properly check that the directory is valid
    true
  }

  private def executePrograms(cus: Seq[CompilationUnit]): Unit = {

    val cusWithMainMethods = cus.filter(_.classes.exists(_.methods.exists(_.isMain)))
    val sources = cusWithMainMethods map { _.source.get }
    if (options(JSONFlag)) {
      val programExecutor = DefaultProgramExecutor(ctx.allClassPaths)
      val results = sources map { programExecutor(_) }

      ctx.output += ExecutionResultOutput(sources zip results)
    } else {
      sources foreach executeStreamingProgram
    }
  }

  private def executeStreamingProgram(source: Source): Unit = {
    import formatter._

    val boxLines = grid
      .header(Bold("Executing program"))
      .content(source.description)
      .row(Column(width = Fixed(6)), Column)
      .contents("", "")
      .render()
      .lines
      .toList
    val programExecutor = StreamingProgramExecutor(ctx.allClassPaths, printExecLine)
    boxLines.take(4).foreach(println)
    val res = programExecutor(source)
    println(boxLines.last)

    if (res.exception.isDefined) {
      val stackTrace = removeCompilerPartOfStacktrace(source.mainName, stackTraceHighlighter(res.exception.get))
      grid
        .header((Red + Bold) ("There was an exception"))
        .row()
        .content(stackTrace)
        .print()
    }
  }

  private def printExecLine(line: String, lineNumber: Int): Unit = {
    import formatter._

    val s = grid
      .row(Column(width = Fixed(6), overflowHandling = OverflowHandling.Truncate), Column)
      .content(Magenta(lineNumber), syntaxHighlighter(line))
      .removeTop()
      .removeBottom()
      .render()
    print(s)
  }

  private def removeCompilerPartOfStacktrace(fileName: String, stackTrace: String) = {
    val stackTraceLines = stackTrace.lines.toList
    val lastRow = stackTraceLines.lastIndexWhere(_.contains(fileName))
    if (lastRow == -1 || lastRow + 1 >= stackTraceLines.length)
      stackTrace
    else
      stackTraceLines.take(lastRow + 1).mkString(NL)
  }

  private def printExecutionTimes(success: Boolean): Unit = {
    if (options(VerboseFlag))
      ctx.output += ExecutionTimeOutput(ctx.executionTimes.toMap, success)
  }

  private def tryExit(code: Int) = throw ExitException(code)

  private def exit(code: Int): Nothing = {
    ctx.output += SuccessOutput(code == 0)
    ctx.output.flush()
    sys.runtime.halt(code)
    throw new RuntimeException("")
  }

  private def error(message: String): Nothing = {
    ctx.output += ErrorOutput(message)
    exit(1)
  }

  private def ErrorNoSourcesGiven(): Nothing =
    error(s"No compilation sources given.")

  private def ErrorInvalidTHomeDirectory(path: String): Nothing =
    error(s"'$path' is not a valid $THome directory.")
}
