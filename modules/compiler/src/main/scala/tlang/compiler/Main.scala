package tlang.compiler

import better.files.{File, FileMonitor}
import cafebabe.{CodeFreezingException, CodegenerationStackTrace}
import tlang.Constants._
import tlang.compiler.analyzer.{Flowing, Naming, Typing}
import tlang.compiler.argument._
import tlang.compiler.ast.Parsing
import tlang.compiler.ast.Trees._
import tlang.compiler.code.{CodeGeneration, Lowering}
import tlang.compiler.imports.ClassPath
import tlang.compiler.lexer.Lexing
import tlang.compiler.messages.{CompilationException, CompilerMessages, DefaultReporter, MessageFormatter}
import tlang.compiler.modification.Templating
import tlang.compiler.utils.{DebugOutputFormatter, TLangSyntaxHighlighter}
import tlang.formatting.grid.Alignment.Center
import tlang.formatting.grid._
import tlang.formatting.textformatters._
import tlang.formatting.{ErrorStringContext, Formatter, Formatting}
import tlang.options.argument._
import tlang.options.{FlagArgument, Options}
import tlang.utils.Extensions._
import tlang.utils._


object Main extends Logging {


  val FrontEnd: CompilerPhase[Source, CompilationUnit] =
    Lexing andThen Parsing andThen Templating andThen Naming andThen Typing andThen Flowing

  val GenerateCode: CompilerPhase[CompilationUnit, CodegenerationStackTrace] =
    Lowering andThen CodeGeneration

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

  case class ExitException(code: Int) extends Throwable
  def tryExit(code: Int) = throw ExitException(code)

  def main(args: Array[String]) {
    val options = parseOptions(args)
    val formatting = Formatting(options)
    val formatter = Formatter(formatting, TLangSyntaxHighlighter(formatting))

    Logging.DefaultLogSettings.formatter = formatter
    Logging.DefaultLogSettings.logLevel = options(LogLevelFlag)
    Logging.DefaultLogSettings.logThreads = options(ThreadsFlag).isInstanceOf[ParallellExecutor]

    if (args.isEmpty) {
      print(printHelpInfo(formatter))
      sys.exit(1)
    }

    printHelp(formatter, options)

    if (!isValidTHomeDirectory(TDirectory))
      ErrorInvalidTHomeDirectory(TDirectory, THome)

    val filesToCompile = options(TFilesArgument)

    if (filesToCompile.isEmpty)
      ErrorNoFilesGiven()

    info"Compiling ${ filesToCompile.size } files: ${ filesToCompile.map(_.path.relativePWD).mkString(NL) }"

    if (options(VerboseFlag))
      printFilesToCompile(formatter, filesToCompile)

    val ctx = createContext(options, formatter)

    compileAndExecute(filesToCompile, options, ctx)

    if (options(WatchFlag))
      startWatchers(filesToCompile, options, ctx)
  }

  private def startWatchers(filesToCompile: Set[File], options: Options, ctx: Context): Unit = {
    val formatter = ctx.formatter
    import formatter.formatting._

    import scala.concurrent.ExecutionContext.Implicits.global

    info"Starting file watchers"

    if (options(VerboseFlag))
      formatter
        .grid
        .header(s"Watching for ${ Green("changes") }...")
        .print()

    case class CompilerFileMonitor(file: File) extends FileMonitor(file, file.isDirectory) {
      override def onModify(file: File, count: Int): Unit = {
        info"$file changed, recompiling"
        if (options(VerboseFlag))
          formatter
            .grid
            .header(s"Found changes to file ${ Magenta(file.path.relativePWD) }, recompiling...")
            .print()

        Source.clearCache()
        ctx.reporter.clear()

        compileAndExecute(Set(file), options, ctx)
      }
    }

    filesToCompile
      .map { file =>
        info"Watching file $file for changes"
        CompilerFileMonitor(file)
      }
      .foreach { _.start() }

    // Block indefinitely
    Thread.currentThread.join()
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

  private def compileAndExecute(filesToCompile: Set[File], options: Options, ctx: Context): Unit = {
    try {
      val CUs = runCompiler(filesToCompile, options, ctx)

      if (options(VerboseFlag))
        ctx.printExecutionTimes(success = true)

      if (options(ExecFlag))
        executePrograms(ctx, CUs)
    } catch {
      case ExitException(code) => if (!options(WatchFlag)) sys.exit(code)
    }
  }

  private def runCompiler(filesToCompile: Set[File], options: Options, ctx: Context): Seq[CompilationUnit] = {
    try {
      val CUs = runFrontend(filesToCompile, options, ctx)
      GenerateCode.execute(ctx)(CUs)
      ctx.reporter.printWarnings()
      CUs
    } catch {
      case e: ExitException => throw e
      case e: Throwable     => unknownError(ctx, options, e)
    }
  }

  private def runFrontend(filesToCompile: Set[File], options: Options, ctx: Context): List[CompilationUnit] = {
    try {
      val sources = filesToCompile.toList.map(FileSource(_))
      FrontEnd.execute(ctx)(sources)
    } catch {
      case e: CompilationException =>
        e.messages.print()
        if (options(VerboseFlag))
          ctx.printExecutionTimes(success = false)
        tryExit(1)
    }
  }

  // Top level error handling for any unknown exception
  private def unknownError(ctx: Context, options: Options, error: Throwable): Nothing = {
    val formatter = ctx.formatter
    import formatter.formatting._

    val stackTrace = formatter.highlightStackTrace(error)
    error"Execution error occurred: $stackTrace"

    val grid = formatter
      .grid
      .header(s"${ Bold }Compilation ${ Red("failed") }${ Bold(" with an unexpected error") }")
      .row()
      .content(stackTrace)

    // Handling of special errors
    error match {
      case CodeFreezingException(_, Some(stackTrace)) =>
        grid
          .row(alignment = Center)
          .content(Bold(Blue("Stack trace at time of code freezing")))
          .emptyLine()
          .content(stackTrace.header)
          .row(5)
          .columnHeaders("Line", "PC", "Height", "ByteCode", "Info")
          .contents(stackTrace.content)
      case _                                          =>
    }

    grid.print()

    if (options(VerboseFlag))
      ctx.printExecutionTimes(success = false)

    tryExit(1)
  }

  private def createContext(options: Options, formatter: Formatter): Context = {
    info"Creating context with options $options"

    val messageFormatter = MessageFormatter(formatter, TabReplacer(2), options(MessageContextFlag))
    val messages = CompilerMessages(
      formatter,
      messageFormatter,
      maxErrors = options(MaxErrorsFlag),
      warningIsError = options(WarningIsErrorFlag),
      suppressWarnings = options(SuppressWarningsFlag)
    )
    val debugOutputFormatter = DebugOutputFormatter(formatter)
    val executor = options(ThreadsFlag)
    Context(
      reporter = DefaultReporter(messages = messages),
      formatter = formatter,
      debugOutputFormatter = debugOutputFormatter,
      classPath = ClassPath.Default ++ options(ClassPathFlag),
      executor = executor,
      outDirs = options(DirectoryFlag),
      printCodePhase = options(PrintOutputFlag),
      ignoredImports = options(IgnoredDefaultImportsFlag)
    )
  }

  private def printFilesToCompile(formatter: Formatter, files: Set[File]): Unit = {
    val formatting = formatter.formatting
    import formatting._

    val numFiles = files.size
    val end = if (numFiles > 1) "files" else "file"

    val grid = formatter.grid.header(Bold("Compiling") + " " + Blue(numFiles) + " " + Bold(end))

    val fileNames = files.toList.map(formatter.fileName).sorted
    grid
      .row(CenteredColumn)
      .content(EvenlySpaced(fileNames))
    grid.print()
  }

  private def versionInfo = s"T-Compiler $VersionNumber"

  private def printHelp(formatter: Formatter, options: Options): Unit = {
    if (options(VersionFlag)) {
      print(versionInfo)
      sys.exit()
    }
    val args = options(CompilerHelpFlag)

    if (args.contains("all")) {
      printHelpInfo(formatter)
      sys.exit()
    }

    if (args.contains(CompilerHelpFlag.Phases)) {
      printPhaseInfo(formatter)
    }

    args.foreach(arg => CompilerFlags.find(_.name == arg) ifDefined { flag =>
      printFlagInfo(flag, formatter)
    })

    if (args.nonEmpty) {
      sys.exit()
    }
  }

  private def printFlagInfo(flag: FlagArgument[_], formatter: Formatter): Unit = {
    val formatting = formatter.formatting
    formatter
      .grid
      .header(flag.flagName(formatting))
      .row()
      .content(flag.extendedDescription(formatter))
      .print()
  }

  private def printHelpInfo(formatter: Formatter): Unit = {
    val formatting = formatter.formatting
    import formatting._

    val tcomp = Green("tcomp")
    val options = Blue("options")
    val source = Blue("source files")
    val optionsHeader = Bold(Magenta("Options"))
    val flags = CompilerFlags
      .toList
      .sortBy(_.name)
      .map { flag => (flag.flagName(formatting), flag.description(formatter)) }

    val maxFlagWidth = flags.map(_._1.visibleCharacters).max

    val grid = formatter
      .grid
      .header(s"> $tcomp <$options> <$source> \n\n $optionsHeader")

    flags.foreach { columns =>
      grid.row(Column(width = Width.Fixed(maxFlagWidth)), Column)
      grid.contents(columns)
    }

    grid.print()
  }

  private def printPhaseInfo(formatter: Formatter): Unit = {
    val formatting = formatter.formatting
    import formatting._

    formatter
      .grid
      .header(Bold(s"Phases of the T-Compiler"))
      .row(2)
      .mapContent(CompilerPhases) { phase => (Magenta(phase.phaseName.capitalize), phase.description(formatting)) }
      .print()
  }

  private def isValidTHomeDirectory(path: String): Boolean = {
    // TODO: Make this properly check that the directory is valid
    true
  }

  private def executePrograms(ctx: Context, cus: Seq[CompilationUnit]): Unit = {
    val formatter = ctx.formatter
    import formatter.formatting._

    val cusWithMainMethods = cus.filter(_.classes.exists(_.methods.exists(_.isMain)))
    if (cusWithMainMethods.isEmpty) {
      formatter.grid.header(s"Execution ${ Red("failed") }, none of the given files contains a main method.").print()
      return
    }

    val grid = ctx.formatter
      .grid
      .header(Bold(if (cusWithMainMethods.lengthCompare(1) > 0) "Executing programs" else "Executing program"))

    val programExecutor = ProgramExecutor(ctx.allClassPaths)
    cusWithMainMethods.foreach { executeProgram(_, formatter, programExecutor, grid) }
    grid.print()
  }

  private def executeProgram(cu: CompilationUnit, formatter: Formatter, programExecutor: ProgramExecutor, grid: Grid): Unit = {
    // Guaranteed to have a file source
    val file = cu.source.get.asInstanceOf[FileSource].file

    val result = programExecutor(file)
    addOutputOfProgram(grid, formatter, file, result.output)
    result.exception ifDefined { e => addExceptionOfProgram(grid, formatter, file.name, e) }
  }

  private def addOutputOfProgram(grid: Grid, formatter: Formatter, file: File, output: String): Unit = {
    import formatter.formatting._
    if (output.isEmpty)
      return

    val highlighted = formatter.syntaxHighlight(output)
    val lines = formatter
      .splitWithColors(highlighted)
      .zipWithIndex
      .map { case (line, i) => (Magenta(i + 1), line) }
    grid
      .row(alignment = Center)
      .content(formatter.fileName(file))
      .row(2)
      .contents(lines)
  }

  private def addExceptionOfProgram(grid: Grid, formatter: Formatter, fileName: String, exception: Throwable) = {
    import formatter.formatting._
    val errorColor = Red + Bold
    val stackTrace = removeCompilerPartOfStacktrace(fileName, formatter.highlightStackTrace(exception))
    grid
      .row(alignment = Center)
      .content(errorColor(fileName))
      .row()
      .content(stackTrace)
  }

  private def removeCompilerPartOfStacktrace(fileName: String, stackTrace: String) = {
    val stackTraceLines = stackTrace.lines.toList
    val lastRow = stackTraceLines.lastIndexWhere(_.contains(fileName))
    if (lastRow == -1 || lastRow + 1 >= stackTraceLines.length)
      stackTrace
    else
      stackTraceLines.take(lastRow + 1).mkString(NL)
  }

  private def error(message: String) = {
    println(message)
    sys.exit(1)
  }

  private def ErrorNoFilesGiven(): Nothing =
    error(s"No files given.")

  private def ErrorInvalidTHomeDirectory(path: String, tHome: String): Nothing =
    error(s"'$path' is not a valid $tHome directory.")


}
