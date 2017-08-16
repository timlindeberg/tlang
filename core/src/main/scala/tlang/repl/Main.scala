package tlang.repl

import java.io.File
import java.nio.file.Files

import akka.actor.ActorSystem
import tlang.Context
import tlang.compiler.DebugOutputFormatter
import tlang.compiler.error.{DefaultReporter, ErrorFormatter, ErrorMessages}
import tlang.compiler.imports.ClassPath
import tlang.compiler.options.Flags._
import tlang.compiler.options.Options
import tlang.repl.Repl.{StartRepl, StopRepl}
import tlang.repl.input.InputHistory
import tlang.utils.formatting._

object Main {

  val VersionNumber = "0.0.1"
  val MaxRedoSize   = 500
  val TabSize       = 4


  def main(args: Array[String]): Unit = {

    val options = Options(args)
    if (options(Version)) {
      printVersion()
      sys.exit()
    }

    if (options(Help).nonEmpty) {
      printHelp(options.formatting, options(Help))
      sys.exit()
    }

    val tempDir = Files.createTempDirectory("repl").toFile
    tempDir.deleteOnExit()


    val formatting = options.formatting
    val formatter = Formatter(formatting)
    val errorFormatter = ErrorFormatter(formatter, options(ErrorContext))

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

  private def createContext(options: Options, errorFormatter: ErrorFormatter, tempDir: File): Context = {
    val formatter = errorFormatter.formatter
    val formatting = formatter.formatting
    val default = ClassPath.Default
    val classPath = default ++ (options.classPaths + tempDir.getAbsolutePath)
    val errorMessages = ErrorMessages(errorFormatter, options(MaxErrors))
    val debugOutputFormatter = DebugOutputFormatter(errorFormatter.formatter)
    Context(
      reporter = DefaultReporter(
        suppressWarnings = options(SuppressWarnings),
        warningIsError = options(WarningIsError),
        formatting = formatting,
        messages = errorMessages
      ),
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
