package tlang.repl

import com.googlecode.lanterna.input.{KeyStroke, KeyType}
import com.googlecode.lanterna.terminal.DefaultTerminalFactory
import com.googlecode.lanterna.terminal.swing.{SwingTerminalFontConfiguration, TerminalEmulatorColorConfiguration, TerminalEmulatorPalette}
import tlang.compiler.Context
import tlang.compiler.error.CompilationException

/**
  * Created by Tim Lindeberg on 2/25/2017.
  */
case class ReplLoop(ctx: Context) {

  private val replProgram = ReplProgram(ctx)
  private val terminal    = new ReplTerminal(ctx.formatting)
  private var running     = false

  def start(): Unit = {
    running = true
    val formatting = ctx.formatting

    terminal.onClose {running = false}
    terminal.putWelcomeBox()
    terminal.putInputBox("")
    terminal.flush()

    val commandBuffer = new StringBuilder
    while (running) {
      val keyStroke: KeyStroke = terminal.readInput()

      keyStroke.getKeyType match {
        case KeyType.Enter if keyStroke.isCtrlDown =>
          val command = commandBuffer.toString()
          commandBuffer.clear()
          if (command.nonEmpty) {
            val results = executeCommand(command)
            terminal.putResultBox(command, results)
          }
        case KeyType.Backspace                     =>
          if (commandBuffer.nonEmpty)
            commandBuffer.deleteCharAt(commandBuffer.length - 1)
        case _                                     =>
          val c = keyStroke.getCharacter

          if (c != null)
            commandBuffer += c
      }

      terminal.putInputBox(commandBuffer.toString)
      terminal.flush()
    }
  }


  def executeCommand(command: String): List[String] = {
    if (!command.startsWith(":")) {
      try {
        return replProgram.execute(command)
      } catch {
        case e: CompilationException => return List(e.getMessage)
      }

    }

    command.drop(1) match {
      case "help"  =>
        List("LOL NO HELP FOR U")
      case "quit"  =>
        running = false
        List("Exiting...")
      case "print" =>
        List(replProgram.prettyPrinted)
      case _       => List(s"Command not supported: $command")
    }
  }

  private def createTerminal() =
    new DefaultTerminalFactory()
      .setTerminalEmulatorColorConfiguration(
        TerminalEmulatorColorConfiguration.newInstance(TerminalEmulatorPalette.GNOME_TERMINAL))
      .setTerminalEmulatorFontConfiguration(
        SwingTerminalFontConfiguration.newInstance(new java.awt.Font("Consolas", 0, 22)))
      .createTerminal()

}
