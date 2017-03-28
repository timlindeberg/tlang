package tlang.repl

import akka.actor.{Actor, Props}
import com.googlecode.lanterna.input.{KeyStroke, KeyType}
import tlang.compiler.error.Formatting
import tlang.repl.InputHandler.SaveToFile
import tlang.repl.Renderer.{Failure, Loading, Success}
import tlang.repl.Repl.Stop
import tlang.repl.ReplProgram.{Execute, PrettyPrint, StopExecution}
import tlang.utils.{Enumerable, Enumeration}

/**
  * Created by Tim Lindeberg on 3/25/2017.
  */

object InputHandler {

  trait InputHandlerMessage

  case class NewInput(keyStroke: KeyStroke) extends InputHandlerMessage
  case object SaveToFile extends InputHandlerMessage

  def props(formatting: Formatting, input: InputHistory) = Props(new InputHandler(formatting, input))
  val name = "inputHandler"
}

class InputHandler(formatting: Formatting, input: InputHistory) extends Actor {

  private def parent = context.parent
  private def current = input.current

  override def receive: Receive = {
    case InputHandler.NewInput(keyStroke) =>

      keyStroke match {
        case Commands.Evaluate()   => Commands.Evaluate(keyStroke)
        case Commands.CancelExec() => Commands.CancelExec(keyStroke)
        case _                     =>
          Commands.find(_.unapply(keyStroke)) match {
            case Some(command) => command(keyStroke)
            case None          =>
              val c = keyStroke.getCharacter
              if (c != null) current += c
          }
          if (!keyStroke.isShiftDown)
            current.setMark()

          parent ! Renderer.NewInput(current)
      }
    case SaveToFile                       => input.saveToFile()
  }

  sealed abstract class Command() extends Product with Serializable {
    def unapply(keyStroke: KeyStroke): Boolean
    def apply(keyStroke: KeyStroke): Unit
  }

  object Commands extends Enumerable[Command] {

    case object Evaluate extends Command {

      override def unapply(keyStroke: KeyStroke): Boolean =
        (keyStroke.getCharacter == ' ' || keyStroke.getCharacter == 128) && keyStroke.isCtrlDown

      override def apply(keyStroke: KeyStroke): Unit = {
        val c = current
        val command = c.toString
        if (command.nonEmpty && !command.forall(_.isWhitespace)) {
          input.saveCurrent()
          parent ! Loading
          evaluate(command)
        }
      }

      private def evaluate(command: String): Unit = {
        import formatting.colors._

        val cmd = command.trim
        if (!cmd.startsWith(":")) {
          parent ! Execute(command)
          return
        }
        cmd.drop(1).toLowerCase match {
          case "help"          => parent ! Success("Help message", truncate = false)
          case "quit" | "exit" => parent ! Stop
          case "print"         => parent ! PrettyPrint
          case _               => parent ! Failure(Bold("Command not supported: ") + Red(command), truncate = false)
        }
      }
    }

    case object Undo extends Command {
      override def unapply(keyStroke: KeyStroke): Boolean =
        keyStroke.isCtrlDown && !keyStroke.isAltDown && keyStroke.getCharacter == 'z'

      override def apply(keyStroke: KeyStroke): Unit = current.undo()
    }

    case object Redo extends Command {
      override def unapply(keyStroke: KeyStroke): Boolean =
        keyStroke.isCtrlDown && keyStroke.isAltDown && keyStroke.getCharacter == 'z'

      override def apply(keyStroke: KeyStroke): Unit = current.redo()
    }

    case object Remove extends Command {
      override def unapply(keyStroke: KeyStroke): Boolean = keyStroke.getKeyType == KeyType.Backspace

      override def apply(keyStroke: KeyStroke): Unit =
        if (largeMovement(keyStroke)) current.removeToLeftWord() else current.remove()
    }

    case object GoLeft extends Command {
      override def unapply(keyStroke: KeyStroke): Boolean = keyStroke.getKeyType == KeyType.ArrowLeft

      override def apply(keyStroke: KeyStroke): Unit =
        if (largeMovement(keyStroke)) current.goToLeftWord() else current.moveLeft(1)

    }

    case object GoRight extends Command {
      override def unapply(keyStroke: KeyStroke): Boolean = keyStroke.getKeyType == KeyType.ArrowRight

      override def apply(keyStroke: KeyStroke): Unit =
        if (largeMovement(keyStroke)) current.goToRightWord() else current.moveRight(1)

    }

    case object GoUp extends Command {
      override def unapply(keyStroke: KeyStroke): Boolean = keyStroke.getKeyType == KeyType.ArrowUp

      override def apply(keyStroke: KeyStroke): Unit =
        if (!current.up())
          input.goToPrevious()
    }

    case object GoDown extends Command {
      override def unapply(keyStroke: KeyStroke): Boolean = keyStroke.getKeyType == KeyType.ArrowDown

      override def apply(keyStroke: KeyStroke): Unit =
        if (!current.down())
          input.goToNext()
    }

    //case object Copy extends Command {
    //  override def matches(keyStroke: KeyStroke): Boolean = keyStroke.isCtrlDown && keyStroke.getCharacter == 'c'
    //  override def execute(keyStroke: KeyStroke): Unit = current.copy()
    //}

    case object Paste extends Command {
      override def unapply(keyStroke: KeyStroke): Boolean = keyStroke.isCtrlDown && keyStroke.getCharacter == 'v'
      override def apply(keyStroke: KeyStroke): Unit = current.paste()
    }

    case object Cut extends Command {
      override def unapply(keyStroke: KeyStroke): Boolean = keyStroke.isCtrlDown && keyStroke.getCharacter == 'x'
      override def apply(keyStroke: KeyStroke): Unit = current.cut()
    }

    case object CancelExec extends Command {
      override def unapply(keyStroke: KeyStroke): Boolean = keyStroke.isCtrlDown && keyStroke.getCharacter == 'c'
      override def apply(keyStroke: KeyStroke): Unit = parent ! StopExecution
    }

    private def largeMovement(keyStroke: KeyStroke): Boolean = keyStroke.isAltDown

    protected lazy val All: Set[Command] = Enumeration.instancesOf[Command]
  }


}
