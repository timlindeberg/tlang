package tlang
package compiler

import tlang.compiler.argument._
import tlang.compiler.ast.Trees._
import tlang.compiler.execution.{Compiler, CompilerFileWatcher, Executor, ProgramExecutor}
import tlang.compiler.imports.ClassPath
import tlang.compiler.messages._
import tlang.compiler.output._
import tlang.compiler.output.help.{FlagInfoOutput, HelpOutput, PhaseInfoOutput, VersionOutput}
import tlang.compiler.utils.TLangSyntaxHighlighter
import tlang.formatting.textformatters.{StackTraceHighlighter, SyntaxHighlighter}
import tlang.formatting.{ErrorStringContext, Formatter}
import tlang.options.argument._
import tlang.options.{FlagArgument, Options}
import tlang.utils._

object CompilerMain extends Logging {
  val Flags: Set[FlagArgument[_]] = Set(
    AsciiFlag,
    ClassPathFlag,
    ColorSchemeFlag,
    CompilerHelpFlag,
    DirectoryFlag,
    ExecFlag,
    ExecTimeoutFlag,
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
    CompilerMain(ctx).run()
  }

  private def parseOptions(args: Array[String]): Options = {
    implicit val errorContext: ErrorStringContext = ErrorStringContext()(Formatter.SimpleFormatter)
    try {
      Options(flags = Flags, positionalArgument = Some(TFilesArgument), arguments = args)
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

case class CompilerMain(ctx: Context) extends Logging {

  import CompilerMain._
  import ctx.{formatter, options}

  private implicit val syntaxHighlighter: SyntaxHighlighter = TLangSyntaxHighlighter()
  private implicit val stackTraceHighlighter: StackTraceHighlighter = StackTraceHighlighter()

  private val interruptionHandler = InterruptionHandler()
  private val programExecutor = ProgramExecutor(ctx, interruptionHandler)
  private val compiler = Compiler(ctx)
  private val executor = Executor(ctx, interruptionHandler)

  def run(): Unit = {
    if (options.isEmpty) {
      ctx.output += HelpOutput(Constants.CompilerCommandName, Flags)
      executor.exit(1)
    }

    printHelp()

    val sources = getSources
    if (sources.isEmpty)
      executor.error(s"No compilation sources given.")

    info"Compiling ${ sources.size } sources: ${ sources.map { _.description }.mkString(NL) }"

    if (options(VerboseFlag))
      ctx.output += SourcesOutput(sources)

    val CUs = compileAndExecute(sources)

    if (!options(WatchFlag))
      executor.exit(0)

    CompilerFileWatcher(ctx, options, sources, compileAndExecute, CUs).watch()
  }

  private def getSources: List[Source] = {
    options(TFilesArgument).map(FileSource(_)).toList ++ sourceFromStdin
  }

  private def sourceFromStdin(): List[Source] = {
    if (options(ReadStdinFlag)) List(StdinSource()) else List()
  }

  private def compileAndExecute(sources: List[Source]): Seq[CompilationUnit] = {
    executor.execute {
      val CUs = compiler(sources)
      printExecutionTimes(success = true)
      if (options(ExecFlag))
        programExecutor(CUs)
      CUs
    }.getOrElse(Nil)
  }

  private def printHelp(): Unit = {
    if (options(VersionFlag)) {
      ctx.output += VersionOutput()
      executor.exit(0)
    }
    val args = options(CompilerHelpFlag)

    if (HelpFlag.defaultArg in args) {
      ctx.output += HelpOutput(Constants.CompilerCommandName, Flags)
      executor.exit(0)
    }

    if (CompilerHelpFlag.Phases in args) {
      ctx.output += PhaseInfoOutput(Compiler.Phases)
    }

    args foreach { arg =>
      Flags.find(_.name == arg) ifDefined { ctx.output += FlagInfoOutput(_) }
    }

    if (args.nonEmpty)
      executor.exit(0)
  }

  private def printExecutionTimes(success: Boolean): Unit = {
    if (options(VerboseFlag))
      ctx.output += ExecutionTimeOutput(ctx.executionTimes.toMap, success)
  }
}
