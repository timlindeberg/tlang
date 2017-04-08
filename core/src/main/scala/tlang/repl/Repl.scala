package tlang.repl

import akka.actor.{Actor, Props}
import com.googlecode.lanterna.input.{KeyStroke, KeyType}
import tlang.compiler.Context
import tlang.repl.Renderer._
import tlang.repl.ReplProgram._
import tlang.repl.input.InputHistory
import tlang.utils.{Enumerable, Enumeration}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._


trait State
case object AwaitingExecution extends State
case object Normal extends State


object Repl {
  case object StartRepl
  case object StopRepl
  case class SetState(state: State)

  def props(ctx: Context, replTerminal: ReplTerminal, inputHistory: InputHistory) =
    Props(new Repl(ctx, replTerminal, inputHistory))

  val name = "repl"
}


sealed abstract class Command() extends Product with Serializable {
  def unapply(keyStroke: KeyStroke): Boolean
  def apply(keyStroke: KeyStroke): Boolean
}

class Repl(ctx: Context, terminal: ReplTerminal, inputHistory: InputHistory) extends Actor {

  import Repl._
  import ctx.formatting._

  private val MaxOutputLines  = 10
  private val LoadingInterval = ctx.formatting.spinner.frameTime.length

  private val renderer    = context.actorOf(Renderer.props(ctx.formatting, MaxOutputLines, terminal), Renderer.name)
  private val replProgram = context.actorOf(ReplProgram.props(ctx, MaxOutputLines), ReplProgram.name)

  private def currentInput = inputHistory.current

  private var state: State = Normal

  override def receive: Receive = {
    case SetState(state)         =>
      println(s"Got message: SetState($state) from $sender")
      this.state = state
    case StartRepl               =>
      println(s"Got message: StartRepl from $sender")
      replProgram ! Warmup
      renderer ! Renderer.StartRepl
      awaitInput()
    case StopRepl                =>
      println(s"Got message: StopRepl from $sender")
      renderer ! Renderer.StopRepl
      terminal.close()
      inputHistory.saveToFile()
      context.system.terminate()
    case msg: RendererMessage    => renderer forward msg
    case msg: ReplProgramMessage => replProgram forward msg
  }

  private def awaitInput(): Unit = {
    Future { terminal.readInput() } onSuccess {
      case key: KeyStroke if key.getKeyType == KeyType.EOF => self ! StopRepl
      case keyStroke: KeyStroke                            =>
        println(s"Got input: $keyStroke")
        keyStroke match {
          case Commands.Evaluate()   => Commands.Evaluate(keyStroke)
          case Commands.CancelExec() => Commands.CancelExec(keyStroke)
          case _                     =>
            val updateRenderer = Commands.find(_.unapply(keyStroke)) match {
              case Some(command) => command(keyStroke)
              case None          =>
                val validInput = state == Normal && keyStroke.getCharacter != null
                if (validInput)
                  currentInput += keyStroke.getCharacter
                validInput
            }

            if (updateRenderer) {
              if (!keyStroke.isShiftDown)
                currentInput.setMark()

              renderer ! Renderer.DrawNewInput(inputHistory.current)
            }

        }
        awaitInput()
    }
  }

  private def setAwaitExecution(): Unit = {
    terminal.setCursorVisible(false)
    state = AwaitingExecution
    Future {
      //noinspection LoopVariableNotUpdated
      while (state == AwaitingExecution) {
        Thread.sleep(LoadingInterval)
        if (state == AwaitingExecution) {
          renderer ! DrawLoading
        }
      }
    }
  }

  object Commands extends Enumerable[Command] {

    // These are matched against in the order that their defined

    case object Evaluate extends Command {

      override def unapply(keyStroke: KeyStroke): Boolean =
        state == Normal &&
        keyStroke.isCtrlDown &&
        (keyStroke.getCharacter == ' ' || keyStroke.getCharacter == 128)

      override def apply(keyStroke: KeyStroke): Boolean = {
        val command = currentInput.toString
        if (command.nonEmpty && !command.forall(_.isWhitespace)) {
          inputHistory.saveCurrent()
          evaluate(command)
        }
        false
      }

      private def evaluate(command: String): Unit = {
        val cmd = command.trim
        if (!cmd.startsWith(":")) {
          setAwaitExecution()
          replProgram ! Execute(command)
          return
        }
        cmd.drop(1).toLowerCase match {
          case "help"          => renderer ! DrawSuccess("Help message", truncate = false)
          case "quit" | "exit" => self ! StopRepl
          case "print"         => replProgram ! PrettyPrint
          case _               => renderer ! DrawFailure(Bold("Command not supported: ") + Red(command), truncate = false)
        }
      }
    }

    case object Undo extends Command {
      override def unapply(keyStroke: KeyStroke): Boolean = {
        state == Normal && keyStroke.isCtrlDown && keyStroke.getCharacter == 'z'
      }

      override def apply(keyStroke: KeyStroke): Boolean = currentInput.undo()
    }

    case object Redo extends Command {
      override def unapply(keyStroke: KeyStroke): Boolean = {
        state == Normal && keyStroke.isCtrlDown && keyStroke.isAltDown && keyStroke.getCharacter == 'z'
      }

      override def apply(keyStroke: KeyStroke): Boolean = currentInput.redo()
    }

    case object Remove extends Command {
      override def unapply(keyStroke: KeyStroke): Boolean = {
        state == Normal && keyStroke.getKeyType == KeyType.Backspace
      }

      override def apply(keyStroke: KeyStroke): Boolean = {
        if (largeMovement(keyStroke)) currentInput.removeToLeftWord() else currentInput.remove()
        true
      }
    }

    case object GoLeft extends Command {
      override def unapply(keyStroke: KeyStroke): Boolean = {
        state == Normal && keyStroke.getKeyType == KeyType.ArrowLeft
      }

      override def apply(keyStroke: KeyStroke): Boolean = {
        if (largeMovement(keyStroke)) currentInput.goToLeftWord() else currentInput.moveLeft(1)
        true
      }

    }

    case object GoRight extends Command {
      override def unapply(keyStroke: KeyStroke): Boolean = {
        state == Normal && keyStroke.getKeyType == KeyType.ArrowRight
      }

      override def apply(keyStroke: KeyStroke): Boolean = {
        if (largeMovement(keyStroke)) currentInput.goToRightWord() else currentInput.moveRight(1)
        true
      }

    }

    case object GoUp extends Command {
      override def unapply(keyStroke: KeyStroke): Boolean = {
        state == Normal && keyStroke.getKeyType == KeyType.ArrowUp
      }

      override def apply(keyStroke: KeyStroke): Boolean = {
        if (!currentInput.up())
          inputHistory.goToPrevious()
        true
      }

    }

    case object GoDown extends Command {
      override def unapply(keyStroke: KeyStroke): Boolean = {
        state == Normal && keyStroke.getKeyType == KeyType.ArrowDown
      }

      override def apply(keyStroke: KeyStroke): Boolean = {
        if (!currentInput.down())
          inputHistory.goToNext()
        true
      }

    }

    //case object Copy extends Command {
    //  override def matches(keyStroke: KeyStroke): Boolean = keyStroke.isCtrlDown && keyStroke.getCharacter == 'c'
    //  override def execute(keyStroke: KeyStroke): Unit = currentInput.copy()
    //}

    case object Paste extends Command {
      override def unapply(keyStroke: KeyStroke): Boolean = {
        state == Normal && keyStroke.isCtrlDown && keyStroke.getCharacter == 'v'
      }

      override def apply(keyStroke: KeyStroke): Boolean = {
        currentInput.paste()
        true
      }
    }

    case object Cut extends Command {
      override def unapply(keyStroke: KeyStroke): Boolean = {
        state == Normal && keyStroke.isCtrlDown && keyStroke.getCharacter == 'x'
      }

      override def apply(keyStroke: KeyStroke): Boolean = {
        currentInput.cut()
        true
      }
    }

    case object CancelExec extends Command {
      override def unapply(keyStroke: KeyStroke): Boolean = {
        keyStroke.isCtrlDown && keyStroke.getCharacter == 'c'
      }
      override def apply(keyStroke: KeyStroke): Boolean = {
        state match {
          case AwaitingExecution =>
            replProgram ! StopExecution
            state = Normal
          case Normal            => self ! StopRepl
        }
        false
      }
    }

    private def largeMovement(keyStroke: KeyStroke): Boolean = keyStroke.isAltDown

    protected lazy val All: List[Command] = Enumeration.instancesOf[Command]
  }

}
