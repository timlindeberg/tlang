package tlang.repl

import java.awt.Toolkit
import java.awt.datatransfer.{Clipboard, DataFlavor}

import com.googlecode.lanterna.input.{KeyStroke, KeyType}
import tlang.compiler.Context
import tlang.compiler.error.{CompilationException, ErrorMessages}
import tlang.utils.Extensions._
import tlang.utils.{Enumerable, Enumeration}

import scala.concurrent.{TimeoutException, duration}

/**
  * Created by Tim Lindeberg on 2/25/2017.
  */
case class ReplLoop(ctx: Context) {

  private val MaxRedoSize = 500
  private val TabSize     = 4
  private val Timeout     = duration.Duration(2, "sec")

  private val replProgram = ReplProgram(ctx, Timeout)
  private val terminal    = new ReplTerminal(ctx.formatting)
  private var running     = false
  private val commands    = CommandHistory(MaxRedoSize, TabSize)

  def start(): Unit = {

    sys.addShutdownHook {
      commands.saveToFile()
    }

    running = true

    terminal.onClose {running = false}
    terminal.putWelcomeBox()

    try {
      loop()
    } finally {
      commands.saveToFile()
    }
  }

  private def loop() =
    while (running) {
      terminal.putInputBox(commands.current)
      terminal.flush()

      val keyStroke = terminal.readInput()

      Commands.find(_.matches(keyStroke)) match {
        case Some(command) => command.execute(keyStroke)
        case None          =>
          val c = keyStroke.getCharacter
          if (c != null) commands.current += c
      }
    }

  sealed abstract class Command() extends Product with Serializable {
    def matches(keyStroke: KeyStroke): Boolean
    def execute(keyStroke: KeyStroke): Unit
  }

  object Commands extends Enumerable[Command] {

    trait EvaluationResult

    case class CompilationFailed(messages: ErrorMessages) extends EvaluationResult
    case class Failure(message: String) extends EvaluationResult
    case class Success(message: String) extends EvaluationResult

    private def largeMovement(keyStroke: KeyStroke): Boolean = keyStroke.isAltDown

    case object Evaluate extends Command {

      override def matches(keyStroke: KeyStroke): Boolean =
        keyStroke.getKeyType == KeyType.Enter && keyStroke.isCtrlDown

      override def execute(keyStroke: KeyStroke): Unit = {
        val command = commands.current.text
        if (command.nonEmpty) {
          commands.saveCurrent()
          evaluate(command) match {
            case Success(message)            => terminal.putResultBox(command, message, success = true)
            case Failure(message)            => terminal.putResultBox(command, message, success = false)
            case CompilationFailed(messages) => terminal.putErrorBox(command, messages.getErrors)
          }
        }
      }

      def evaluate(command: String): EvaluationResult = {
        import ctx.formatting.colors._

        if (!command.startsWith(":")) {
          try {
            return Success(replProgram.execute(command))
          } catch {
            case e: CompilationException => return CompilationFailed(e.messages)
            case _: TimeoutException     =>
              val msg = Bold("Execution timed out after " + Red(Timeout)) + Bold(".")
              return Failure(msg)
          }

        }

        val message = command.drop(1) match {
          case "help"  => "LOL NO HELP FOR U"
          case "quit"  =>
            running = false
            "Exiting..."
          case "print" =>
            replProgram.prettyPrinted.trimWhiteSpaces
          case _       =>
            return Failure(Bold + "Command not supported: " + Red(command))
        }
        Success(message)
      }

    }

    case object Undo extends Command {
      override def matches(keyStroke: KeyStroke): Boolean =
        keyStroke.isCtrlDown && !keyStroke.isAltDown && keyStroke.getCharacter == 'z'

      override def execute(keyStroke: KeyStroke): Unit = commands.current.undo()
    }

    case object Redo extends Command {
      override def matches(keyStroke: KeyStroke): Boolean =
        keyStroke.isCtrlDown && keyStroke.isAltDown && keyStroke.getCharacter == 'z'

      override def execute(keyStroke: KeyStroke): Unit = commands.current.redo()
    }


    case object Remove extends Command {
      override def matches(keyStroke: KeyStroke): Boolean = keyStroke.getKeyType == KeyType.Backspace

      override def execute(keyStroke: KeyStroke): Unit =
        if (largeMovement(keyStroke)) commands.current.removeToLeftWord() else commands.current.remove()
    }

    case object GoLeft extends Command {
      override def matches(keyStroke: KeyStroke): Boolean = keyStroke.getKeyType == KeyType.ArrowLeft

      override def execute(keyStroke: KeyStroke): Unit = {
        if (largeMovement(keyStroke)) commands.current.goToLeftWord() else commands.current.moveLeft(1)
      }
    }

    case object GoRight extends Command {
      override def matches(keyStroke: KeyStroke): Boolean = keyStroke.getKeyType == KeyType.ArrowRight

      override def execute(keyStroke: KeyStroke): Unit = {
        if (largeMovement(keyStroke)) commands.current.goToRightWord() else commands.current.moveRight(1)
      }
    }

    case object GoUp extends Command {
      override def matches(keyStroke: KeyStroke): Boolean = keyStroke.getKeyType == KeyType.ArrowUp

      override def execute(keyStroke: KeyStroke): Unit = {
        if (!commands.current.up())
          commands.goToPrevious()
      }
    }

    case object GoDown extends Command {
      override def matches(keyStroke: KeyStroke): Boolean = keyStroke.getKeyType == KeyType.ArrowDown

      override def execute(keyStroke: KeyStroke): Unit = {
        if (!commands.current.down())
          commands.goToNext()
      }
    }

    case object Paste extends Command {

      override def matches(keyStroke: KeyStroke): Boolean = keyStroke.isCtrlDown && keyStroke.getCharacter == 'v'

      override def execute(keyStroke: KeyStroke): Unit = commands.current ++= clipboardContents

      private val systemClipboard: Clipboard = Toolkit.getDefaultToolkit.getSystemClipboard
      private def clipboardContents: String = systemClipboard.getData(DataFlavor.stringFlavor).asInstanceOf[String]
    }

    protected lazy val All: Set[Command] = Enumeration.instancesOf[Command]
  }


}
