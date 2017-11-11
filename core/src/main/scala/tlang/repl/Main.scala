package tlang.repl

import akka.actor.{ActorRef, ActorSystem}
import better.files._
import com.googlecode.lanterna.terminal.Terminal
import com.typesafe.config.ConfigFactory
import tlang.Context
import tlang.compiler.DebugOutputFormatter
import tlang.compiler.Main.CompilerFlags
import tlang.compiler.ast.PrettyPrinter
import tlang.compiler.code.TreeBuilder
import tlang.compiler.imports.{ClassPath, Imports}
import tlang.formatting._
import tlang.formatting.textformatters.TabReplacer
import tlang.messages._
import tlang.options.arguments._
import tlang.options.{FlagArgument, Options}
import tlang.repl.actors.RenderingActor.Resize
import tlang.repl.actors.ReplActor
import tlang.repl.actors.ReplActor.{Start, Stop}
import tlang.repl.evaluation.{Evaluator, Extractor, ReplState, SaveAndPrintTransformer}
import tlang.repl.input.{Clipboard, Input}
import tlang.repl.terminal.{KeyConverter, ReplTerminal, TerminalFactory}
import tlang.utils.{Logging, ProgramExecutor}


object Main {

  val VersionNumber   = "0.0.1"
  val MaxRedoSize     = 500
  val TabWidth        = 3
  val DoubleClickTime = 500L


  val HistoryFileName  : String = "repl_history"
  val SettingsDirectory: File   = System.getProperty("user.home") / ".tlang"

  val ReplFlags: List[FlagArgument[_]] = List(
    LineWidthFlag,
    AsciiFlag,
    ClassPathFlag,
    VersionFlag,
    ReplHelpFlag,
    MessageContextFlag//,
   // LogLevelFlag
  )

  def main(args: Array[String]): Unit = {

    val options = parseOptions(args)
    val formatting = Formatting(options)

    if (options(VersionFlag)) {
      printVersion()
      sys.exit()
    }

    if (options(ReplHelpFlag).nonEmpty) {
      printHelp(formatting, options(ReplHelpFlag))
      sys.exit()
    }

    val terminal = TerminalFactory.createTerminal()
    val repl = createRepl(terminal, options, formatting)
    repl ! Start
  }

  def createRepl(terminal: Terminal, options: Options, formatting: Formatting): ActorRef = {
    // Inject dependencies

    val formatter = Formatter(formatting)
    val errorFormatter = MessageFormatter(formatter, TabReplacer(2), options(MessageContextFlag))

    Logging.DefaultLogSettings.logLevel = options(LogLevelFlag)

    val tempDir = File.newTemporaryDirectory("repl")

    val context = createContext(options, errorFormatter, tempDir)


    val prettyPrinter = PrettyPrinter(formatting)
    val errorStringContext = ErrorStringContext(formatter)
    val replState = ReplState(prettyPrinter, Imports(context, errorStringContext))


    val extractor = Extractor(formatter, replState)
    val programExecutor = ProgramExecutor(context)
    val statementTransformer = SaveAndPrintTransformer(TreeBuilder(), replState)
    val evaluator = Evaluator(context, extractor, programExecutor, statementTransformer, replState)

    val tabReplacer = TabReplacer(TabWidth)
    val messageFormatter = MessageFormatter(formatter, tabReplacer)


    val keyConverter = KeyConverter(DoubleClickTime)
    val replTerminal = ReplTerminal(terminal, keyConverter, formatting, TabWidth)
    replTerminal.enableMouseReporting = true

    val historyFile = File(SettingsDirectory, HistoryFileName)
    val input = Input(historyFile, Clipboard(), MaxRedoSize, TabWidth)

    val singleMessageMailboxConfig = ConfigFactory.parseString(
      """rendererMailbox.mailbox-type = "tlang.repl.actors.SingleMessageMailbox""""
    )
    val actorSystem = ActorSystem("tRepl", singleMessageMailboxConfig)
    val outputBox = OutputBox(formatter, tabReplacer, errorFormatter, maxOutputLines = 5)
    val repl = actorSystem.actorOf(
      ReplActor.props(replState, evaluator, formatter, outputBox, replTerminal, input),
      ReplActor.name
    )

    terminal.addResizeListener((_, newSize) => {
      val width = newSize.getColumns
      if (formatting.lineWidth != width) {
        formatting.lineWidth = width
        repl ! Resize(width)
      }
    })

    // In case were using a Swing terminal
    replTerminal onClose { repl ! Stop }
    repl
  }


  private def parseOptions(args: Array[String]): Options = {
    val formatter = Formatter(SimpleFormatting)

    val errorContext = ErrorStringContext(formatter)
    Options(flags = CompilerFlags, positionalArgument = Some(TFilesArgument), arguments = args)(errorContext)
  }

  private def printVersion(): Unit = println(s"T-Repl $VersionNumber")

  private def createContext(options: Options, errorFormatter: MessageFormatter, tempDir: File): Context = {
    val formatter = errorFormatter.formatter
    val classPath = ClassPath.Default ++ (options(ClassPathFlag) + tempDir.pathAsString)

    val errorMessages = CompilerMessages(formatter, errorFormatter, maxErrors = 5)
    val debugOutputFormatter = DebugOutputFormatter(formatter)
    Context(
      reporter = DefaultReporter(errorMessages),
      formatter = formatter,
      debugOutputFormatter = debugOutputFormatter,
      classPath = classPath,
      outDirs = Set(tempDir)
    )
  }


  private def printHelp(formatting: Formatting, args: Set[String] = Set("")) = {
    println("TODO")
    sys.exit(0)
  }

}
