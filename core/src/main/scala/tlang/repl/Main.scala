package tlang.repl

import java.io.File
import java.nio.file.Files

import akka.actor.ActorSystem
import tlang.Context
import tlang.compiler.DebugOutputFormatter
import tlang.compiler.Main.CompilerFlags
import tlang.compiler.ast.PrettyPrinter
import tlang.compiler.imports.ClassPath
import tlang.formatting._
import tlang.messages._
import tlang.options.arguments._
import tlang.options.{FlagArgument, Options}
import tlang.repl.Repl.{StartRepl, StopRepl}
import tlang.repl.input.InputHistory

object Main {

  val VersionNumber = "0.0.1"
  val MaxRedoSize   = 500
  val TabSize       = 4

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

    val tempDir = Files.createTempDirectory("repl").toFile
    tempDir.deleteOnExit()


    val formatter = Formatter(formatting)
    val errorFormatter = MessageFormatter(formatter, options(MessageContextFlag))

    val context = createContext(options, errorFormatter, tempDir)

    val actorSystem = ActorSystem("tRepl")

    val replTerminal = ReplTerminal()
    val inputHistory = InputHistory(MaxRedoSize, TabSize)
    val prettyPrinter = PrettyPrinter(formatting)
    val repl = actorSystem.actorOf(Repl.props(context, errorFormatter, prettyPrinter, replTerminal, inputHistory), Repl.name)

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
    val classPath = ClassPath.Default ++ (options(ClassPathFlag) + tempDir.getAbsolutePath)

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
