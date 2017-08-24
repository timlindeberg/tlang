package tlang.repl

import java.io.File
import java.nio.file.Files

import akka.actor.ActorSystem
import tlang.Context
import tlang.compiler.DebugOutputFormatter
import tlang.compiler.error.{CompilerMessages, DefaultReporter, MessageFormatter}
import tlang.compiler.imports.ClassPath
import tlang.formatting._
import tlang.options.Arguments._
import tlang.options.{Arguments, Options}
import tlang.repl.Repl.{StartRepl, StopRepl}
import tlang.repl.input.InputHistory

object Main {

  val VersionNumber = "0.0.1"
  val MaxRedoSize   = 500
  val TabSize       = 4

  val ReplFlags: List[Arguments.FlagArgument[_]] = List(
    LineWidthFlag,
    FormattingStyleFlag,
    ClassPathFlag,
    VersionFlag,
    HelpFlag,
    MessageContextFlag
  )

  def main(args: Array[String]): Unit = {

    val options = Options(ReplFlags, None, args)
    val formatting = Formatting(options)

    if (options(VersionFlag)) {
      printVersion()
      sys.exit()
    }

    if (options(HelpFlag).nonEmpty) {
      printHelp(formatting, options(HelpFlag))
      sys.exit()
    }

    val tempDir = Files.createTempDirectory("repl").toFile
    tempDir.deleteOnExit()


    val formatter = Formatter(formatting)
    val errorFormatter = MessageFormatter(formatter, options(MessageContextFlag))

    val context = createContext(options, errorFormatter, tempDir)

    val actorSystem = ActorSystem("tRepl")

    val replTerminal = new ReplTerminal
    val inputHistory = InputHistory(MaxRedoSize, TabSize)
    val repl = actorSystem.actorOf(Repl.props(context, errorFormatter, replTerminal, inputHistory), Repl.name)

    // In case were using a Swing terminal
    replTerminal onClose { repl ! StopRepl }

    repl ! StartRepl
  }

  private def printVersion(): Unit = println(s"T-Repl $VersionNumber")

  private def createContext(options: Options, errorFormatter: MessageFormatter, tempDir: File): Context = {
    val formatter = errorFormatter.formatter
    val formatting = formatter.formatting
    val classPath = ClassPath.Default ++ (options(ClassPathFlag) + tempDir.getAbsolutePath)

    val errorMessages = CompilerMessages(formatter, errorFormatter, maxErrors = 5)
    val debugOutputFormatter = DebugOutputFormatter(errorFormatter.formatter)
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
