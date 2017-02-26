package tlang.repl

import com.googlecode.lanterna.input.{KeyStroke, KeyType}
import tlang.compiler.Context
import tlang.compiler.error.CompilationException
import tlang.utils.Enumeration

/**
  * Created by Tim Lindeberg on 2/25/2017.
  */
case class ReplLoop(ctx: Context) {

  private val replProgram   = ReplProgram(ctx)
  private val terminal      = new ReplTerminal(ctx.formatting)
  private var running       = false
  private val commandBuffer = CommandBuffer(50)

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
        case Some(command) => command.execute()
        case None          =>
          val c = keyStroke.getCharacter
          if (c != null) commandBuffer += c
      }
      println("commandBuffer: " + commandBuffer)

      terminal.putInputBox(commandBuffer)
      terminal.flush()
    }
  }

  object Commands {

    sealed abstract class Command() extends Product with Serializable {
      def matches(keyStroke: KeyStroke): Boolean
      def execute(): Unit
    }

    case object Evaluate extends Command {
      override def matches(keyStroke: KeyStroke): Boolean =
        keyStroke.getKeyType == KeyType.Enter && keyStroke.isCtrlDown

      override def execute(): Unit = {
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

    }

    case object RemoveOne extends Command {
      override def matches(keyStroke: KeyStroke): Boolean = keyStroke.getKeyType == KeyType.Backspace
      override def execute(): Unit = commandBuffer.remove()
    }

    case object Undo extends Command {
      override def matches(keyStroke: KeyStroke): Boolean = {
        val c = keyStroke.getCharacter
        if (c == null)
          return false

        c == 'z' && keyStroke.isCtrlDown && !keyStroke.isAltDown
      }
      override def execute(): Unit = commandBuffer.undo()
    }

    case object Redo extends Command {
      override def matches(keyStroke: KeyStroke): Boolean = {
        val c = keyStroke.getCharacter
        if (c == null)
          return false

        c == 'z' && keyStroke.isCtrlDown && keyStroke.isAltDown
      }
      override def execute(): Unit = commandBuffer.redo()
    }

    case object Left extends Command {
      override def matches(keyStroke: KeyStroke): Boolean = keyStroke.getKeyType == KeyType.ArrowLeft
      override def execute(): Unit = commandBuffer.left(1)
    }

    case object Right extends Command {
      override def matches(keyStroke: KeyStroke): Boolean = keyStroke.getKeyType == KeyType.ArrowRight
      override def execute(): Unit = commandBuffer.right(1)
    }

    case object Up extends Command {
      override def matches(keyStroke: KeyStroke): Boolean = keyStroke.getKeyType == KeyType.ArrowUp
      override def execute(): Unit = commandBuffer.up()
    }

    case object Down extends Command {
      override def matches(keyStroke: KeyStroke): Boolean = keyStroke.getKeyType == KeyType.ArrowDown
      override def execute(): Unit = commandBuffer.down()
    }

    lazy val All: Set[Command] = Enumeration.instancesOf[Command]

  }


}
