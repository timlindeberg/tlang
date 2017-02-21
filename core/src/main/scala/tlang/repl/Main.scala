package tlang.repl

import com.googlecode.lanterna.input.{KeyStroke, KeyType}
import com.googlecode.lanterna.terminal._

/**
  * Created by Tim Lindeberg on 2/13/2017.
  */
object Repl {

  var running = true

  val replProgram        = ReplProgram()
  val terminal: Terminal = new DefaultTerminalFactory().createTerminal()

  def main(args: Array[String]): Unit = {

    val commandBuffer = new StringBuilder
    while (running) {
      val keyStroke: KeyStroke = terminal.readInput()

      keyStroke match {
        case CommandEntry() =>
          val command = commandBuffer.toString()
          commandBuffer.clear()
          executeCommand(command)
        case Backspace()    =>
          if (commandBuffer.nonEmpty)
            commandBuffer.deleteCharAt(commandBuffer.length - 1)
        case _              =>
          commandBuffer += keyStroke.getCharacter
      }

      terminal.clearScreen()
      commandBuffer.foreach(terminal.putCharacter)
      terminal.flush()
    }
  }

  def executeCommand(command: String): Unit = {
    if (!command.startsWith(":")) {
      //replProgram.handleCommand(command)
      return
    }
    command.drop(1) match {
      case "quit" => running = false
      case _      => println("Input command: " + command)
    }
  }

  object CommandEntry {
    def unapply(keyStroke: KeyStroke): Boolean = keyStroke.isCtrlDown && keyStroke.getKeyType == KeyType.Enter
  }
  object Backspace {
    def unapply(keyStroke: KeyStroke): Boolean = keyStroke.getKeyType == KeyType.Backspace
  }

  implicit class KeyStrokePimping(val keyStroke: KeyStroke) extends AnyVal {

    def isCommandEntry: Boolean = keyStroke.isCtrlDown && keyStroke.getKeyType == KeyType.Enter
    def isCommandCompletion: Boolean = keyStroke.isCtrlDown && keyStroke.getKeyType == KeyType.Tab

  }


}
