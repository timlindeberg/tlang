package tlang.compiler

import better.files.{File, FileMonitor}
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
import tlang.formatting.textformatters._
import tlang.formatting.{ErrorStringContext, Formatter, Formatting}
import tlang.options.argument._
import tlang.options.{FlagArgument, Options}
import tlang.utils.Extensions._
import tlang.utils._

case class ExitException(code: Int) extends Throwable

object Main extends Logging {

  val FrontEnd: CompilerPhase[Source, CompilationUnit] =
    Lexing andThen Parsing andThen Templating andThen Naming andThen Typing andThen Flowing

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
    SuppressWarningsFlag,
    ThreadsFlag,
    VerboseFlag,
    VersionFlag,
    WarningIsErrorFlag,
    WatchFlag
  )

  def main(args: Array[String]): Unit = {
    val options = parseOptions(args)
    val formatting = Formatting(
      lineWidth = options(LineWidthFlag),
      colorScheme = options(ColorSchemeFlag),
      useColor = !(options(NoColorFlag) || options(JSONFlag)),
      asciiOnly = options(AsciiFlag)
    )
    val formatter = Formatter(formatting, TLangSyntaxHighlighter(formatting))

    Logging.DefaultLogSettings.formatter = formatter
    Logging.DefaultLogSettings.logLevel = options(LogLevelFlag)
    Logging.DefaultLogSettings.logThreads = options(ThreadsFlag).isInstanceOf[ParallellExecutor]

    val ctx = createContext(options, formatter)
    Main(ctx).run()
  }

  private def parseOptions(args: Array[String]): Options = {
    val errorContext = ErrorStringContext()
    try {
      Options(flags = CompilerFlags, positionalArgument = Some(TFilesArgument), arguments = args)(errorContext)
    } catch {
      case e: IllegalArgumentException =>
        println(e.getMessage)
        sys.exit(1)
    }
  }

  private def createContext(options: Options, formatter: Formatter): Context = {
    info"Creating context with options $options"

    val messages = CompilerMessages(
      maxErrors = options(MaxErrorsFlag),
      warningIsError = options(WarningIsErrorFlag),
      suppressWarnings = options(SuppressWarningsFlag)
    )
    val outputHandler = if(options(JSONFlag)) JSONOutputHandler() else PrettyOutputHandler(formatter)
    Context(
      reporter = DefaultReporter(messages = messages),
      formatter = formatter,
      outputHandler = outputHandler,
      classPath = ClassPath.Default ++ options(ClassPathFlag),
      options = options
    )
  }
}


case class Main(ctx: Context) extends Logging {

  private val options = ctx.options
  private val formatter = ctx.formatter
  private val filesToCompile = options(TFilesArgument)
  private val tabReplacer = TabReplacer(2)

  import Main._
  import formatter.formatting._

  def run(): Unit = {
    sys.addShutdownHook {
      ctx.output += MessageOutput("Compilation stopped by signal.")
      exit(0)
    }

    if (options.isEmpty) {
      ctx.output += HelpOutput(CompilerFlags)
      exit(1)
    }

    printHelp()

    if (!isValidTHomeDirectory(TDirectory))
      ErrorInvalidTHomeDirectory(TDirectory, THome)

    if (filesToCompile.isEmpty)
      ErrorNoFilesGiven()

    info"Compiling ${ filesToCompile.size } files: ${ filesToCompile.map { _.path.relativePWD }.mkString(NL) }"

    if (options(VerboseFlag))
      ctx.output += FilesToCompileOutput(filesToCompile)

    compileAndExecute(filesToCompile)

    if (options(WatchFlag))
      watch()
    exit(0)
  }

  private def compileAndExecute(files: Set[File]): Unit = {
    try {
      val CUs = runCompiler(files)
      printExecutionTimes(success = true)
      if (options(ExecFlag))
        executePrograms(CUs)
    } catch {
      case ExitException(code) => exit(code)
    }
  }

  private def runCompiler(files: Set[File]): Seq[CompilationUnit] = {
    try {
      val CUs = runFrontend(files)
      GenerateCode.execute(ctx)(CUs)
      val messages = ctx.reporter.messages
      ctx.output += ErrorMessageOutput(messages, tabReplacer, options(MessageContextFlag), List(MessageType.Warning))
      CUs
    } catch {
      case e: ExitException => throw e
      case e: Throwable     => internalError(e)
    }
  }

  private def runFrontend(files: Set[File]): List[CompilationUnit] = {
    try {
      val sources = files.toList.map(FileSource(_))
      FrontEnd.execute(ctx)(sources)
    } catch {
      case e: CompilationException =>
        ctx.output += ErrorMessageOutput(e.messages, tabReplacer, options(MessageContextFlag))
        printExecutionTimes(success = false)
        tryExit(1)
    }
  }

  // Top level error handling for any unknown exception
  private def internalError(error: Throwable): Nothing = {
    error"Execution error occurred: ${error.stackTrace}"

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
    val programExecutor = ProgramExecutor(ctx.allClassPaths)

    val cusWithMainMethods = cus.filter(_.classes.exists(_.methods.exists(_.isMain)))
    val files = cusWithMainMethods map { _.source.get.asInstanceOf[FileSource].file }
    val results = files map { programExecutor(_) }

    ctx.output += ExecutionResultOutput(files zip results)
  }

  private def watch(): Unit = {
    import scala.concurrent.ExecutionContext.Implicits.global

    info"Starting file watchers"

    if (options(VerboseFlag))
      ctx.output += MessageOutput(s"Watching for ${ Green("changes") }...")

    case class CompilerFileMonitor(file: File) extends FileMonitor(file, file.isDirectory) {
      override def onModify(file: File, count: Int): Unit = {
        info"$file changed, recompiling"
        if (options(VerboseFlag))
          ctx.output += MessageOutput(s"Found changes to file ${ Magenta(file.path.relativePWD) }, recompiling...")

        Source.clearCache()
        ctx.reporter.clear()

        compileAndExecute(Set(file))
      }
    }

    filesToCompile
      .map { file =>
        info"Watching file $file for changes"
        CompilerFileMonitor(file)
      }
      .foreach { _.start() }

    Thread.currentThread().join()
  }

  private def printExecutionTimes(success: Boolean): Unit = {
    if (options(VerboseFlag))
      ctx.output += ExecutionTimeOutput(ctx.executionTimes.toMap, success = false)
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

  private def ErrorNoFilesGiven(): Nothing =
    error(s"No files given.")

  private def ErrorInvalidTHomeDirectory(path: String, tHome: String): Nothing =
    error(s"'$path' is not a valid $tHome directory.")

}
