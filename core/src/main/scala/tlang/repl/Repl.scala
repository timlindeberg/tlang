package tlang.repl

import akka.actor.{Actor, Props}
import com.googlecode.lanterna.input.{KeyStroke, KeyType}
import tlang.Context
import tlang.compiler.error.ErrorFormatter
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

  def props(ctx: Context, errorFormatter: ErrorFormatter, replTerminal: ReplTerminal, inputHistory: InputHistory) =
    Props(new Repl(ctx, errorFormatter, replTerminal, inputHistory))

  val name = "repl"
}


sealed abstract class Command() extends Product with Serializable {
  def unapply(keyStroke: KeyStroke): Boolean
  def apply(keyStroke: KeyStroke): Boolean
  def order: Int
}

class Repl(ctx: Context, errorFormatter: ErrorFormatter, terminal: ReplTerminal, inputHistory: InputHistory) extends Actor {

  import Repl._
  import ctx.formatter.formatting._

  private val MaxOutputLines  = 10
  private val LoadingInterval = ctx.formatting.spinner.frameTime.length

  private val renderer    = context.actorOf(Renderer.props(ctx.formatter, errorFormatter, MaxOutputLines, terminal), Renderer.name)
  private val replProgram = context.actorOf(ReplProgram.props(ctx, MaxOutputLines), ReplProgram.name)

  private def currentInput = inputHistory.current

  private var state: State = Normal

  override def receive: Receive = {
    case SetState(state)         =>
      this.state = state
    case StartRepl               =>
      replProgram ! Warmup
      renderer ! Renderer.StartRepl
      awaitInput()
    case StopRepl                =>
      renderer ! Renderer.StopRepl
      terminal.close()
      inputHistory.saveToFile()
      context.system.terminate()
    case msg: RendererMessage    => renderer forward msg
    case msg: ReplProgramMessage => replProgram forward msg
  }

  private def awaitInput(): Unit = {
    Future { terminal.readInput() } foreach {
      case key: KeyStroke if key.getKeyType == KeyType.EOF => self ! StopRepl
      case keyStroke: KeyStroke                            =>
        val shouldUpdateRenderer = Commands.find(_.unapply(keyStroke)) match {
          case Some(command) => command(keyStroke)
          case None          => false
        }

        if (shouldUpdateRenderer) {
          if (!keyStroke.isShiftDown)
            currentInput.setMark()

          renderer ! Renderer.DrawNewInput(inputHistory.current)
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

      override val order = 1
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

      override val order = 1

      override def unapply(keyStroke: KeyStroke): Boolean =
        state == Normal &&
          keyStroke.isCtrlDown &&
          keyStroke.getCharacter == 'z'

      override def apply(keyStroke: KeyStroke): Boolean = currentInput.undo()
    }

    case object Redo extends Command {

      override val order = 1

      override def unapply(keyStroke: KeyStroke): Boolean =
        state == Normal &&
          keyStroke.isCtrlDown &&
          keyStroke.isAltDown &&
          keyStroke.getCharacter == 'z'

      override def apply(keyStroke: KeyStroke): Boolean = currentInput.redo()
    }

    case object Remove extends Command {

      override val order = 1

      override def unapply(keyStroke: KeyStroke): Boolean =
        state == Normal &&
          keyStroke.getKeyType == KeyType.Backspace

      override def apply(keyStroke: KeyStroke): Boolean = {
        if (largeMovement(keyStroke)) currentInput.removeToLeftWord() else currentInput.removeOne()
        true
      }
    }

    case object GoLeft extends Command {

      override val order = 1

      override def unapply(keyStroke: KeyStroke): Boolean =
        state == Normal &&
          keyStroke.getKeyType == KeyType.ArrowLeft


      override def apply(keyStroke: KeyStroke): Boolean = {
        if (largeMovement(keyStroke)) currentInput.goToLeftWord() else currentInput.moveCursorLeft(1)
        true
      }

    }

    case object GoRight extends Command {

      override val order = 1

      override def unapply(keyStroke: KeyStroke): Boolean =
        state == Normal &&
          keyStroke.getKeyType == KeyType.ArrowRight


      override def apply(keyStroke: KeyStroke): Boolean = {
        if (largeMovement(keyStroke)) currentInput.goToRightWord() else currentInput.moveCursorRight(1)
        true
      }

    }

    case object GoUp extends Command {

      override val order = 1

      override def unapply(keyStroke: KeyStroke): Boolean =
        state == Normal &&
          keyStroke.getKeyType == KeyType.ArrowUp

      override def apply(keyStroke: KeyStroke): Boolean = {
        if (!currentInput.moveCursorUp())
          inputHistory.goToPrevious()
        true
      }

    }

    case object GoDown extends Command {

      override val order = 1

      override def unapply(keyStroke: KeyStroke): Boolean =
        state == Normal &&
          keyStroke.getKeyType == KeyType.ArrowDown

      override def apply(keyStroke: KeyStroke): Boolean = {
        if (!currentInput.moveCursorDown())
          inputHistory.goToNext()
        true
      }

    }

    case object Copy extends Command {

      override val order = 1

      override def unapply(keyStroke: KeyStroke): Boolean =
        state == Normal &&
          keyStroke.isCtrlDown &&
          keyStroke.isAltDown &&
          keyStroke.getCharacter == 'c'

      override def apply(keyStroke: KeyStroke): Boolean = {
        currentInput.copy()
        false
      }
    }

    case object Paste extends Command {

      override val order = 1

      override def unapply(keyStroke: KeyStroke): Boolean =
        state == Normal &&
          keyStroke.isCtrlDown &&
          keyStroke.isAltDown &&
          keyStroke.getCharacter == 'v'

      override def apply(keyStroke: KeyStroke): Boolean = {
        currentInput.paste()
        true
      }
    }

    case object Cut extends Command {

      override val order = 1

      override def unapply(keyStroke: KeyStroke): Boolean =
        state == Normal &&
          keyStroke.isCtrlDown &&
          keyStroke.isAltDown &&
          keyStroke.getCharacter == 'x'


      override def apply(keyStroke: KeyStroke): Boolean = {
        currentInput.cut()
        true
      }
    }

    case object CancelExec extends Command {

      override val order = 2

      override def unapply(keyStroke: KeyStroke): Boolean =
        keyStroke.isCtrlDown &&
          keyStroke.getCharacter == 'c'

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

    case object NewCharacter extends Command {

      override val order = 3

      override def unapply(keyStroke: KeyStroke): Boolean =
        state == Normal &&
          keyStroke.getCharacter != null

      override def apply(keyStroke: KeyStroke): Boolean = {
        currentInput += keyStroke.getCharacter
        true
      }
    }

    private def largeMovement(keyStroke: KeyStroke): Boolean = keyStroke.isAltDown

    protected lazy val All: List[Command] = Enumeration.instancesOf[Command].sortBy(_.order)
  }

}
