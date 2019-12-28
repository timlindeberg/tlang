package tlang
package filetester

import better.files.File
import tlang.compiler.Context
import tlang.compiler.argument._
import tlang.compiler.execution.{Compiler, CompilerFileWatcher, TopLevelExecutor}
import tlang.compiler.imports.ClassPath
import tlang.compiler.messages.{CompilerMessages, DefaultReporter}
import tlang.compiler.output.help.HelpOutput
import tlang.compiler.output.{JSONOutputHandler, PrettyOutputHandler}
import tlang.compiler.utils.TLangSyntaxHighlighter
import tlang.filetester.argument.KeepClassFilesFlag
import tlang.formatting.textformatters.{StackTraceHighlighter, SyntaxHighlighter}
import tlang.formatting.{ErrorStringContext, Formatter}
import tlang.options.argument._
import tlang.options.{FlagArgument, Options}
import tlang.testutils.TestConstants
import tlang.testutils.TestConstants.TestOutputDirectory
import tlang.utils.InterruptionHandler.Category
import tlang.utils.{FileSource, InterruptionHandler, Logging, Source}

object TestFileMain extends Logging {
  case object TestHelpFlag extends HelpFlag(Flags)

  val Flags: Set[FlagArgument[_]] = Set(
    AsciiFlag,
    ClassPathFlag,
    ColorSchemeFlag,
    JSONFlag,
    KeepClassFilesFlag,
    LineWidthFlag,
    LogLevelFlag,
    MessageContextFlag,
    NoColorFlag,
    PrintOutputFlag,
    TabWidthFlag,
    VerboseFlag,
    TestHelpFlag,
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

    val ctx = createContext(options)
    TestFileMain(ctx).run()
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

    val messages = CompilerMessages(maxErrors = -1)
    Context(
      reporter = DefaultReporter(messages = messages),
      output = if (options(JSONFlag)) JSONOutputHandler() else PrettyOutputHandler(),
      outDirs = Set(File(TestConstants.TestOutputDirectory)),
      classPath = ClassPath.Default ++ options(ClassPathFlag),
      printCodePhase = options(PrintOutputFlag),
      options = options
    )
  }
}

case class TestFileMain(ctx: Context) extends Logging {

  import ctx.{formatter, options}

  private implicit val stackTraceHighlighter: StackTraceHighlighter = StackTraceHighlighter()
  private implicit val syntaxHighlighter: SyntaxHighlighter = TLangSyntaxHighlighter()

  private val interruptionHandler = InterruptionHandler()
  private val topLevel = TopLevelExecutor(ctx, interruptionHandler)

  def run(): Unit = {
    interruptionHandler.setHandler(Category("TestFile"), deleteOutputDirectory _)

    val helpArgs = options(CompilerHelpFlag)

    if (options.isEmpty || (HelpFlag.defaultArg in helpArgs)) {
      ctx.output += HelpOutput(Constants.TesterCommandName, TestFileMain.Flags)
      topLevel.exit(1)
    }

    val sources = options(TFilesArgument).map(FileSource(_)).toList
    if (sources.isEmpty)
      topLevel.error(s"No compilation sources given.")

    deleteOutputDirectory()

    val success = testFiles(sources)

    if (!options(WatchFlag))
      topLevel.exit(if (success) 0 else 1)

    CompilerFileWatcher(ctx, options, sources, testFiles).watch()
  }

  private def testFiles(sources: List[Source]): Boolean = {
    val fileSources = sources.asInstanceOf[List[FileSource]]
    topLevel
      .execute {
        // We want to execute all test so we map first since
        // forall will exit early
        fileSources
          .map { testFile }
          .forall(res => res)
      }
      .getOrElse(false)
  }

  private def testFile(source: FileSource): Boolean = {
    info"Executing test on file ${ source.file }"
    val fileTester = CompilerFileTester(source.file, ctx, Compiler.FrontEnd)
    val res = fileTester.execute()
    ctx.output += TestFileOutput(source, res)
    deleteOutputDirectory()

    res.success
  }

  private def deleteOutputDirectory(): Unit = {
    if (options(KeepClassFilesFlag))
      return

    info"Deleting output directory"
    File(TestOutputDirectory).delete(swallowIOExceptions = true)
  }
}
