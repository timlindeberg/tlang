package tlang.repl

import java.awt.Toolkit
import java.awt.datatransfer.{Clipboard, DataFlavor}

import com.googlecode.lanterna.input.{KeyStroke, KeyType}
import tlang.compiler.Context
import tlang.compiler.error.{CompilationException, ErrorMessages}
import tlang.utils.Extensions._
import tlang.utils.{Enumerable, Enumeration}

/**
  * Created by Tim Lindeberg on 2/25/2017.
  */
case class ReplLoop(ctx: Context) {

  private val MaxRedoSize = 500
  private val TabSize     = 4

  private val replProgram = ReplProgram(ctx)
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

    private def largeMovement(keyStroke: KeyStroke): Boolean = keyStroke.isAltDown

    case object Evaluate extends Command {

      override def matches(keyStroke: KeyStroke): Boolean =
        keyStroke.getKeyType == KeyType.Enter && keyStroke.isCtrlDown

      override def execute(keyStroke: KeyStroke): Unit = {
        val command = commands.current.text
        if (command.nonEmpty) {
          commands.saveCurrent()
          evaluate(command) match {
            case Right(messages)     => terminal.putResultBox(command, messages)
            case Left(errorMessages) => terminal.putErrorBox(command, errorMessages.getErrors)
          }
        }
      }

      def evaluate(command: String): Either[ErrorMessages, List[String]] = {
        if (!command.startsWith(":")) {
          try {
            return Right(replProgram.execute(command))
          } catch {
            case e: CompilationException => return Left(e.messages)
          }

        }

        val message = command.drop(1) match {
          case "help"  => "LOL NO HELP FOR U"
          case "quit"  =>
            running = false
            "Exiting..."
          case "print" =>
            replProgram.prettyPrinted.trimWhiteSpaces
          case _       => s"Command not supported: $command"
        }
        Right(List(message))
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
