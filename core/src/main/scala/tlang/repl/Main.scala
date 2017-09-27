package tlang.repl

import java.nio.charset.Charset

import akka.actor.ActorSystem
import better.files._
import com.googlecode.lanterna.TerminalSize
import com.googlecode.lanterna.TextColor.ANSI
import com.googlecode.lanterna.terminal.ansi.{UnixLikeTerminal, UnixTerminal}
import com.googlecode.lanterna.terminal.swing.TerminalEmulatorDeviceConfiguration.CursorStyle
import com.googlecode.lanterna.terminal.swing._
import com.googlecode.lanterna.terminal.{DefaultTerminalFactory, Terminal}
import tlang.Context
import tlang.compiler.DebugOutputFormatter
import tlang.compiler.Main.CompilerFlags
import tlang.compiler.ast.PrettyPrinter
import tlang.compiler.imports.ClassPath
import tlang.formatting._
import tlang.messages._
import tlang.options.arguments._
import tlang.options.{FlagArgument, Options}
import tlang.repl.Renderer.Resize
import tlang.repl.Repl.{StartRepl, StopRepl}
import tlang.repl.input.{Clipboard, Input}
import tlang.repl.terminal.{CustomCharacterPatterns, ReplTerminal}
import tlang.utils.Extensions._

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

    val actorSystem = ActorSystem("tRepl")

    val terminal = createUnderlyingTerminal()
    val replTerminal = ReplTerminal(terminal, 500L, formatting)
    replTerminal.enableMouseReporting(true)

    val historyFile = File(SettingsDirectory, HistoryFileName)
    val input = Input(historyFile, Clipboard(), MaxRedoSize)
    val prettyPrinter = PrettyPrinter(formatting)
    val repl = actorSystem.actorOf(Repl.props(context, errorFormatter, prettyPrinter, replTerminal, input), Repl.name)

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

  private def createUnderlyingTerminal(): Terminal = {
    if (sys.env.get("useTerminalEmulator").contains("true"))
      return createTerminalEmulator()

    val charset = Charset.forName(System.getProperty("file.encoding"))
    new UnixTerminal(System.in, System.out, charset, UnixLikeTerminal.CtrlCBehaviour.TRAP) use { term =>
      term.getInputDecoder.addProfile(CustomCharacterPatterns)
    }
  }

  private def createTerminalEmulator() =
    new DefaultTerminalFactory()
      .setTerminalEmulatorColorConfiguration(emulatorColors)
      .setTerminalEmulatorFontConfiguration(emulatorFont)
      .setInitialTerminalSize(new TerminalSize(80, 50))
      .setTerminalEmulatorDeviceConfiguration(deviceConfiguration)
      .createTerminal()

  private lazy val emulatorFont =
    new SwingTerminalFontConfiguration(
      true,
      AWTTerminalFontConfiguration.BoldMode.EVERYTHING,
      new java.awt.Font("Meslo LG S", 0, 14)
    )

  private lazy val deviceConfiguration =
    new TerminalEmulatorDeviceConfiguration(0, 500, CursorStyle.VERTICAL_BAR, ANSI.RED, true)

  private lazy val emulatorColors = TerminalEmulatorColorConfiguration.newInstance(new TerminalEmulatorPalette(
    new java.awt.Color(177, 204, 217), // defaultColor
    new java.awt.Color(177, 204, 217), // defaultBrightColor
    new java.awt.Color(50, 65, 72), //    defaultBackgroundColor
    new java.awt.Color(65, 87, 98), //    normalBlack
    new java.awt.Color(100, 133, 146), // brightBlack
    new java.awt.Color(247, 140, 108), // normalRed
    new java.awt.Color(255, 83, 112), //  brightRed
    new java.awt.Color(195, 232, 141), // normalGreen
    new java.awt.Color(204, 247, 175), // brightGreen
    new java.awt.Color(255, 203, 107), // normalYellow
    new java.awt.Color(255, 203, 67), //  brightYellow
    new java.awt.Color(130, 170, 255), // normalBlue
    new java.awt.Color(137, 221, 255), // brightBlue
    new java.awt.Color(199, 146, 234), // normalMagenta
    new java.awt.Color(207, 160, 237), // brightMagenta
    new java.awt.Color(147, 233, 217), // normalCyan
    new java.awt.Color(0, 185, 204), //   brightCyan
    new java.awt.Color(247, 247, 247), // normalWhite
    new java.awt.Color(255, 255, 255) //  brightWhite
  ))

}
