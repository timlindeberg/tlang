package tlang
package compiler

import tlang.compiler.TestFileMain.Flags
import tlang.compiler.argument._
import tlang.compiler.execution.{Compiler, CompilerFileWatcher, Executor}
import tlang.compiler.imports.ClassPath
import tlang.compiler.messages.{CompilerMessages, DefaultReporter}
import tlang.compiler.output.help.HelpOutput
import tlang.compiler.output.{JSONOutputHandler, MessageOutput, PrettyOutputHandler}
import tlang.compiler.utils.TLangSyntaxHighlighter
import tlang.formatting.textformatters.{StackTraceHighlighter, SyntaxHighlighter}
import tlang.formatting.{ErrorStringContext, Formatter}
import tlang.options.argument._
import tlang.options.{FlagArgument, Options}
import tlang.utils.{FileSource, InterruptionHandler, Logging, Source}

object TestFileMain extends Logging {
  case object TestHelpFlag extends HelpFlag(Flags)

  val Flags: Set[FlagArgument[_]] = Set(
    AsciiFlag,
    ClassPathFlag,
    ColorSchemeFlag,
    JSONFlag,
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
    val outputHandler = if (options(JSONFlag)) JSONOutputHandler() else PrettyOutputHandler()
    Context(
      reporter = DefaultReporter(messages = messages),
      outputHandler = outputHandler,
      classPath = ClassPath.Default ++ options(ClassPathFlag),
      options = options
    )
  }
}

case class TestFileMain(ctx: Context) {

  import ctx.{formatter, options}

  private implicit val stackTraceHighlighter: StackTraceHighlighter = StackTraceHighlighter()
  private implicit val syntaxHighlighter: SyntaxHighlighter = TLangSyntaxHighlighter()

  private val interruptionHandler = InterruptionHandler()
  private val executor = Executor(ctx, interruptionHandler)

  def run(): Unit = {
    val helpArgs = options(CompilerHelpFlag)

    if (options.isEmpty || (HelpFlag.defaultArg in helpArgs)) {
      ctx.output += HelpOutput(Constants.TesterCommandName, Flags)
      executor.exit(1)
    }

    val sources = options(TFilesArgument).map(FileSource(_)).toList
    val success = testFiles(sources)

    if (!options(WatchFlag))
      executor.exit(if (success) 0 else 1)

    CompilerFileWatcher(ctx, options, sources, testFiles).watch()
  }

  private def testFiles(sources: List[Source]): Boolean = {
    executor.execute {
      sources.asInstanceOf[List[FileSource]] forall { testFile }
    }.getOrElse(false)
  }

  private def testFile(source: FileSource): Boolean = {
    import formatter._

    val fileTester = CompilerFileTester(source.file, ctx, Compiler.FrontEnd)
    val res = fileTester.execute()
    if (res.success) {
      ctx.output += MessageOutput(s"${ Green("Test of file") } ${ source.getDescription(Green + Bold) } ${ Green("was successful.") }")
    } else {
      ctx.output += MessageOutput(s"${ Red("Test of file") } ${ source.getDescription(Red + Bold) } ${ Red("failed.") }")
      ctx.output += MessageOutput(res.message.trim)
    }
    res.success
  }
}
