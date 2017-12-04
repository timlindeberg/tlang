package tlang.compiler

import java.lang.reflect.InvocationTargetException

import better.files.File
import cafebabe.CodegenerationStackTrace
import tlang.Constants._
import tlang.Context
import tlang.compiler.analyzer.{Flowing, Naming, Typing}
import tlang.compiler.ast.Parsing
import tlang.compiler.ast.Trees._
import tlang.compiler.code.{CodeGeneration, Lowering}
import tlang.compiler.imports.ClassPath
import tlang.compiler.lexer.Lexing
import tlang.compiler.modification.Templating
import tlang.formatting._
import tlang.formatting.grid.Alignment.Center
import tlang.formatting.grid.Width.Percentage
import tlang.formatting.grid.{Column, Grid, Width}
import tlang.formatting.textformatters.TabReplacer
import tlang.messages._
import tlang.options.arguments._
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
    WarningIsErrorFlag
  )

  def main(args: Array[String]) {

    val options = parseOptions(args)
    val formatting = Formatting(options)
    val formatter = Formatter(formatting)

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

    info"Compiling ${ filesToCompile.size } files: ${ filesToCompile.map(f => '"' + f.toString + '"').mkString(NL) }"

    if (options(VerboseFlag))
      printFilesToCompile(formatter, filesToCompile)

    val ctx = createContext(options, formatter)

    val CUs = runCompiler(filesToCompile, options, ctx)

    if (options(VerboseFlag))
      ctx.printExecutionTimes(success = true)

    if (options(ExecFlag))
      executePrograms(ctx, CUs)
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

  private def runCompiler(filesToCompile: Set[File], options: Options, ctx: Context): Seq[CompilationUnit] = {
    try {
      val CUs = runFrontend(filesToCompile, options, ctx)
      GenerateCode.execute(ctx)(CUs)
      ctx.reporter.printWarnings()
      CUs
    } catch {
      case e: Throwable => unknownError(ctx, options, e)
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
        sys.exit(1)
    }
  }

  // Top level error handling for any unknown exception
  private def unknownError(ctx: Context, options: Options, error: Throwable): Nothing = {
    val formatter = ctx.formatter
    import formatter.formatting._

    val stackTrace = formatter.highlightStackTrace(error)
    error"Execution error occurred: $stackTrace"

    formatter
      .grid
      .header(s"${ Bold }Compilation ${ Red("failed") }${ Bold(" with an unknown error") }")
      .row()
      .content(stackTrace)
      .print()

    if (options(VerboseFlag))
      ctx.printExecutionTimes(success = false)

    sys.exit(1)
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

    val fileNames = files.toList.map(formatter.fileName)
    formatting.lineWidth match {
      case x if x in (0 to 59)  =>
        grid.row().allContent(List(fileNames))
      case x if x in (60 to 99) =>
        grid
          .row(Column(width = Percentage(0.5)), Column(width = Percentage(0.5)))
          .mapContent(fileNames.grouped(2).toList) {
            case f1 :: f2 :: Nil => (f1, f2)
            case f :: Nil        => (f, "")
            case _               => ("", "")
          }
      case x if x >= 100        =>
        grid
          .row(Column(width = Percentage(0.333)), Column(width = Percentage(0.333)), Column(width = Percentage(0.333)))
          .mapContent(fileNames.grouped(3).toList) {
            case f1 :: f2 :: f3 :: _ => (f1, f2, f3)
            case f1 :: f2 :: Nil     => (f1, f2, "")
            case f :: Nil            => (f, "", "")
            case _                   => ("", "", "")
          }
    }
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
      grid.content(columns)
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
    /*
    def listFiles(f: File): Array[File] = {
      val these = f.listFiles
      if (these == null)
        return Array[File]()
      these ++ these.filter(_.isDirectory).flatMap(listFiles)
    }

    val files = listFiles(new File(path))
    val neededFiles = List(
      "T",
      "T/lang",
      "T/lang/Object.t",
      "T/lang/String.t",
      "T/std"
    )
    val fileMap = mutable.Map() ++ neededFiles.map((_, false))
    val filePaths = files.map(_.getAbsolutePath.drop(path.length + 1).replaceAll("\\\\", "/"))
    for (f <- filePaths)
      fileMap(f) = true

    if (fileMap.exists(!_._2))
      return false
    */
    true

  }

  private def executePrograms(ctx: Context, cus: Seq[CompilationUnit]): Unit = {
    val formatter = ctx.formatter
    import formatter.formatting._

    val mainMethods = cus.flatMap(_.classes.flatMap(_.methods.filter(_.isMain)))
    if (mainMethods.isEmpty) {
      println("--exec failed, none of the given files contains a main method.")
      return
    }

    val grid = ctx.formatter
      .grid
      .header(Bold(if (mainMethods.lengthCompare(1) > 0) "Executing programs" else "Executing program"))

    val programExecutor = ProgramExecutor(ctx)
    cus.foreach { executeProgram(_, formatter, programExecutor, grid) }
    grid.print()
  }

  private def executeProgram(cu: CompilationUnit, formatter: Formatter, programExecutor: ProgramExecutor, grid: Grid): Unit = {
    // Guaranteed to have a file source
    val file = cu.source.get.asInstanceOf[FileSource].file

    val output = try {
      programExecutor(file)
    } catch {
      case exception: InvocationTargetException =>
        addExceptionOfProgram(grid, formatter, file.name, exception.getCause)
        return
    }
    addOutputOfProgram(grid, formatter, file, output)
  }

  private def addOutputOfProgram(grid: Grid, formatter: Formatter, file: File, output: String) = {
    import formatter.formatting._
    val lines = formatter.syntaxHighlight(output)
      .split("\r?\n")
      .zipWithIndex
      .map { case (line, i) => (Magenta(i + 1), line) }

    grid
      .row(alignment = Center)
      .content(formatter.fileName(file))
      .row(2)
      .contents(lines)
  }

  private def addExceptionOfProgram(grid: Grid, formatter: Formatter, header: String, exception: Throwable) = {
    import formatter.formatting._
    val errorColor = Red + Bold
    grid
      .row(alignment = Center)
      .content(errorColor(header))
      .row()
      .content(formatter.highlightStackTrace(exception))
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
