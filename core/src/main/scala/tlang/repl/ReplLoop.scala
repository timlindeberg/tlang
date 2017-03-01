package tlang.repl

import java.io.File

import com.googlecode.lanterna.input.{KeyStroke, KeyType}
import tlang.compiler.Context
import tlang.compiler.error.{CompilationException, ErrorLevel}
import tlang.utils.Enumeration
import tlang.utils.Extensions._

/**
  * Created by Tim Lindeberg on 2/25/2017.
  */
case class ReplLoop(ctx: Context) {

  private val settingsDirectory = System.getProperty("user.home") + File.separator + ".tlang"
  private val historyFile       = new File(settingsDirectory + File.separator + "history")
  private val replProgram       = ReplProgram(ctx)
  private val terminal          = new ReplTerminal(ctx.formatting)
  private var running           = false
  private val commandBuffer     = CommandBuffer(maxHistorySize = 50, tabSize = 4)

  def start(): Unit = {
    running = true
    terminal.onClose {running = false}
    terminal.putWelcomeBox()
    terminal.putInputBox(commandBuffer)
    terminal.flush()

    while (running) {
      // Blocking
      val keyStroke = terminal.readInput()

      Commands.All.find(_.matches(keyStroke)) match {
        case Some(command) => command.execute(keyStroke)
        case None          =>
          val c = keyStroke.getCharacter
          if (c != null) commandBuffer += c
      }
      //println("commandBuffer: " + commandBuffer)

      terminal.putInputBox(commandBuffer)
      terminal.flush()
    }
  }

  object Commands {

    sealed abstract class Command() extends Product with Serializable {
      def matches(keyStroke: KeyStroke): Boolean
      def execute(keyStroke: KeyStroke): Unit
    }

    case object Evaluate extends Command {
      override def matches(keyStroke: KeyStroke): Boolean =
        keyStroke.getKeyType == KeyType.Enter && keyStroke.isCtrlDown

      override def execute(keyStroke: KeyStroke): Unit = {
        val command = commandBuffer.command
        commandBuffer.clear()
        if (command.nonEmpty) {
          val results = evaluate(command)

          terminal.putResultBox(command, results)
        }
      }

      def evaluate(command: String): List[String] = {
        if (!command.startsWith(":")) {
          try {
            return replProgram.execute(command)
          } catch {
            case e: CompilationException => return e.messages(ErrorLevel.Error).map(_.msg)
          }

        }

        command.drop(1) match {
          case "help"  =>
            List("LOL NO HELP FOR U")
          case "quit"  =>
            running = false
            List("Exiting...")
          case "print" =>
            List(replProgram.prettyPrinted.trimWhiteSpaces)
          case _       => List(s"Command not supported: $command")
        }
      }

    }

    case object RemoveOne extends Command {
      override def matches(keyStroke: KeyStroke): Boolean = keyStroke.getKeyType == KeyType.Backspace
      override def execute(keyStroke: KeyStroke): Unit = commandBuffer.remove()
    }

    case object Undo extends Command {
      override def matches(keyStroke: KeyStroke): Boolean = {
        val c = keyStroke.getCharacter
        if (c == null)
          return false

        c == 'z' && keyStroke.isCtrlDown && !keyStroke.isAltDown
      }
      override def execute(keyStroke: KeyStroke): Unit = commandBuffer.undo()
    }

    case object Redo extends Command {
      override def matches(keyStroke: KeyStroke): Boolean = {
        val c = keyStroke.getCharacter
        if (c == null)
          return false

        c == 'z' && keyStroke.isCtrlDown && keyStroke.isAltDown
      }
      override def execute(keyStroke: KeyStroke): Unit = commandBuffer.redo()
    }

    case object Left extends Command {
      override def matches(keyStroke: KeyStroke): Boolean = keyStroke.getKeyType == KeyType.ArrowLeft
      override def execute(keyStroke: KeyStroke): Unit = {
        if (keyStroke.isCtrlDown) commandBuffer.leftWord() else commandBuffer.left(1)
      }
    }

    case object Right extends Command {
      override def matches(keyStroke: KeyStroke): Boolean = keyStroke.getKeyType == KeyType.ArrowRight
      override def execute(keyStroke: KeyStroke): Unit = {
        if (keyStroke.isCtrlDown) commandBuffer.rightWord() else commandBuffer.right(1)
      }
    }

    case object Up extends Command {
      override def matches(keyStroke: KeyStroke): Boolean = keyStroke.getKeyType == KeyType.ArrowUp
      override def execute(keyStroke: KeyStroke): Unit = commandBuffer.up()
    }

    case object Down extends Command {
      override def matches(keyStroke: KeyStroke): Boolean = keyStroke.getKeyType == KeyType.ArrowDown
      override def execute(keyStroke: KeyStroke): Unit = commandBuffer.down()
    }

    lazy val All: Set[Command] = Enumeration.instancesOf[Command]

  }


}
