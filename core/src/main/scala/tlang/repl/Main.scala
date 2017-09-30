package tlang.repl

import akka.actor.ActorSystem
import better.files._
import tlang.Context
import tlang.compiler.DebugOutputFormatter
import tlang.compiler.Main.CompilerFlags
import tlang.compiler.ast.PrettyPrinter
import tlang.compiler.code.TreeBuilder
import tlang.compiler.imports.{ClassPath, Imports}
import tlang.formatting._
import tlang.messages._
import tlang.options.arguments._
import tlang.options.{FlagArgument, Options}
import tlang.repl.actors.RenderingActor.Resize
import tlang.repl.actors.ReplActor
import tlang.repl.actors.ReplActor.{StartRepl, StopRepl}
import tlang.repl.evaluation.{Evaluator, Extractor, ReplState, StatementTransformer}
import tlang.repl.input.{Clipboard, Input}
import tlang.repl.terminal.{ReplTerminal, TerminalFactory}
import tlang.utils.Extensions._
import tlang.utils.ProgramExecutor


object Main {

  val VersionNumber   = "0.0.1"
  val MaxRedoSize     = 500
  val TabSize         = 4
  val DoubleClickTime = 500L


  val HistoryFileName  : String = "repl_history"
  val SettingsDirectory: File   = System.getProperty("user.home") / ".tlang"

  val ReplFlags: List[FlagArgument[_]] = List(
    LineWidthFlag,
    AsciiFlag,
    ClassPathFlag,
    VersionFlag,
    ReplHelpFlag,
    MessageContextFlag
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


    val formatter = Formatter(formatting)
    val errorFormatter = MessageFormatter(formatter, options(MessageContextFlag))

    val tempDir = File.newTemporaryDirectory("repl")

    val context = createContext(options, errorFormatter, tempDir)


    val prettyPrinter = PrettyPrinter(formatting)
    val errorStringContext = ErrorStringContext(formatter)

    val replState = ReplState(prettyPrinter, Imports(context, errorStringContext))


    val extractor = Extractor(formatter, replState)
    val programExecutor = ProgramExecutor(context)
    val statementTransformer = StatementTransformer(TreeBuilder(), replState)
    val evaluator = Evaluator(context, extractor, programExecutor, statementTransformer, replState)

    val messageFormatter = MessageFormatter(formatter)

    val terminal = TerminalFactory.createTerminal()
    val replTerminal = ReplTerminal(terminal, 500L, formatting)
    replTerminal.enableMouseReporting(true)

    val historyFile = File(SettingsDirectory, HistoryFileName)
    val input = Input(historyFile, Clipboard(), MaxRedoSize)


    val actorSystem = ActorSystem("tRepl")
    val repl = actorSystem.actorOf(
      ReplActor.props(replState, evaluator, formatter, messageFormatter, replTerminal, input),
      ReplActor.name)

    terminal.addResizeListener((_, newSize) => {
      debug(s"NewSize: $newSize")
      if (formatting.lineWidth != newSize.getColumns) {
        val oldWidth = formatting.lineWidth
        formatting.lineWidth = newSize.getColumns
        repl ! Resize(oldWidth, newSize.getColumns)
      }
    })

    // In case were using a Swing terminal
    replTerminal onClose { repl ! StopRepl }

    repl ! StartRepl
  }


  private def parseOptions(args: Array[String]): Options = {
    val formatter = Formatter(SimpleFormatting)

    val errorContext = ErrorStringContext(formatter)
    Options(flags = CompilerFlags, positionalArgument = Some(TFilesArgument), arguments = args)(errorContext)
  }

  private def printVersion(): Unit = println(s"T-Repl $VersionNumber")

  private def createContext(options: Options, errorFormatter: MessageFormatter, tempDir: File): Context = {
    val formatter = errorFormatter.formatter
    val formatting = formatter.formatting
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
    // TODO
  }

}
