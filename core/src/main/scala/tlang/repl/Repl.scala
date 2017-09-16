package tlang.repl


import akka.actor.{Actor, Props}
import com.googlecode.lanterna.input.KeyType
import tlang.Context
import tlang.compiler.ast.PrettyPrinter
import tlang.messages.MessageFormatter
import tlang.repl.Renderer._
import tlang.repl.ReplProgram._
import tlang.repl.input.Input
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

  def props(ctx: Context, errorFormatter: MessageFormatter, prettyPrinter: PrettyPrinter, terminal: ReplTerminal, inputHistory: Input) =
    Props(new Repl(ctx, errorFormatter, prettyPrinter, terminal, inputHistory))

  val name = "repl"
}

object KeyStroke {
  def apply(k: com.googlecode.lanterna.input.KeyStroke): KeyStroke =
    KeyStroke(k.getKeyType, k.getCharacter, k.isCtrlDown, k.isAltDown, k.isShiftDown)
}

case class KeyStroke(keyType: KeyType, char: Char, ctrlDown: Boolean, altDown: Boolean, shiftDown: Boolean)


sealed abstract class Command() extends Product with Serializable {

  def matches(state: State, keyStroke: KeyStroke): Boolean = state == inState && matchesKey.isDefinedAt(keyStroke)
  def inState: State
  def matchesKey: PartialFunction[KeyStroke, Boolean]
  def apply(keyStroke: KeyStroke): Boolean
  def priority: Int
}

class Repl(ctx: Context, errorFormatter: MessageFormatter, prettyPrinter: PrettyPrinter, terminal: ReplTerminal, input: Input) extends Actor {

  import Repl._
  import ctx.formatter.formatting._

  private val MaxOutputLines  = 10
  private val LoadingInterval = ctx.formatting.spinner.frameTime.length

  private val renderer    = context.actorOf(Renderer.props(ctx.formatter, errorFormatter, MaxOutputLines, terminal), Renderer.name)
  private val replProgram = context.actorOf(ReplProgram.props(ctx, prettyPrinter, MaxOutputLines), ReplProgram.name)

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
      input.saveToFile()
      context.system.terminate()
    case msg: RendererMessage    => renderer forward msg
    case msg: ReplProgramMessage => replProgram forward msg
  }

  private def awaitInput(): Unit = {
    Future { terminal.readInput() } map { KeyStroke(_) } foreach {
      case KeyStroke(KeyType.EOF, _, _, _, _) => self ! StopRepl
      case keyStroke: KeyStroke               =>
        val shouldUpdateRenderer = Commands.find(_.matches(state, keyStroke)) match {
          case Some(command) => command(keyStroke)
          case None          => false
        }

        if (shouldUpdateRenderer)
          renderer ! Renderer.DrawNewInput(input.currentBuffer)

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

    case object Evaluate extends Command {

      override val inState: State = Normal
      override val priority       = 1
      override def matchesKey: PartialFunction[KeyStroke, Boolean] = {
        case KeyStroke(_, (' ' | 128), isCtrlDown@true, _, _) => true
      }

      override def apply(keyStroke: KeyStroke): Boolean = {
        val command = input.toString
        if (command.nonEmpty && !command.forall(_.isWhitespace)) {
          input.saveCurrentCommand()
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

      override val inState: State = Normal
      override val priority       = 1

      override def matchesKey: PartialFunction[KeyStroke, Boolean] = {
        case KeyStroke(_, 'z', isCtrlDown@true, _, _) => true
      }

      override def apply(keyStroke: KeyStroke): Boolean = input.undo()
    }

    case object Redo extends Command {

      override val inState: State = Normal
      override val priority       = 1

      override def matchesKey: PartialFunction[KeyStroke, Boolean] = {
        case KeyStroke(_, 'z', isCtrlDown@true, isAltDown@true, _) => true
      }

      override def apply(keyStroke: KeyStroke): Boolean = input.redo()
    }

    case object Remove extends Command {

      override val inState: State = Normal
      override val priority       = 1

      override def matchesKey: PartialFunction[KeyStroke, Boolean] = {
        case KeyStroke(KeyType.Backspace, _, _, _, _) => true
      }

      override def apply(keyStroke: KeyStroke): Boolean = {
        if (largeMovement(keyStroke)) input.removeToLeftWord() else input.removeSelected()
        true
      }
    }

    case object GoLeft extends Command {

      override val inState: State = Normal
      override val priority       = 1

      override def matchesKey: PartialFunction[KeyStroke, Boolean] = {
        case KeyStroke(KeyType.ArrowLeft, _, _, _, _) => true
      }

      override def apply(keyStroke: KeyStroke): Boolean = {
        val isShiftDown = keyStroke.shiftDown
        if (largeMovement(keyStroke)) input.goToLeftWord(isShiftDown) else input.left(isShiftDown)
        true
      }

    }

    case object GoRight extends Command {

      override val inState: State = Normal
      override val priority       = 1

      override def matchesKey: PartialFunction[KeyStroke, Boolean] = {
        case KeyStroke(KeyType.ArrowRight, _, _, _, _) => true
      }


      override def apply(keyStroke: KeyStroke): Boolean = {
        val isShiftDown = keyStroke.shiftDown
        if (largeMovement(keyStroke)) input.goToRightWord(isShiftDown) else input.right(isShiftDown)
        true
      }

    }

    case object GoUp extends Command {

      override val inState: State = Normal
      override val priority       = 1

      override def matchesKey: PartialFunction[KeyStroke, Boolean] = {
        case KeyStroke(KeyType.ArrowUp, _, _, _, _) => true
      }

      override def apply(keyStroke: KeyStroke): Boolean = {
        input.up(keyStroke.shiftDown)
        true
      }

    }

    case object GoDown extends Command {

      override val inState: State = Normal
      override val priority       = 1

      override def matchesKey: PartialFunction[KeyStroke, Boolean] = {
        case KeyStroke(KeyType.ArrowDown, _, _, _, _) => true
      }

      override def apply(keyStroke: KeyStroke): Boolean = {
        input.down(keyStroke.shiftDown)
        true
      }

    }

    case object Copy extends Command {

      override val inState: State = Normal
      override val priority       = 1

      override def matchesKey: PartialFunction[KeyStroke, Boolean] = {
        case KeyStroke(_, 'c', ctrlDown@true, altDown@true, _) => true
      }

      override def apply(keyStroke: KeyStroke): Boolean = {
        input.copySelected()
        true
      }
    }

    case object Paste extends Command {

      override val inState: State = Normal
      override val priority       = 1

      override def matchesKey: PartialFunction[KeyStroke, Boolean] = {
        case KeyStroke(_, 'v', ctrlDown@true, altDown@true, _) => true
      }

      override def apply(keyStroke: KeyStroke): Boolean = {
        input.paste()
        true
      }
    }

    case object Cut extends Command {

      override val inState: State = Normal
      override val priority       = 1

      override def matchesKey: PartialFunction[KeyStroke, Boolean] = {
        case KeyStroke(_, 'x', ctrlDown@true, altDown@true, _) => true
      }

      override def apply(keyStroke: KeyStroke): Boolean = {
        input.cutSelected()
        true
      }
    }

    case object ExitProgram extends Command {

      override val inState: State = Normal
      override val priority       = 2

      override def matchesKey: PartialFunction[KeyStroke, Boolean] = {
        case KeyStroke(_, 'c', ctrlDown@true, _, _) => true
      }

      override def apply(keyStroke: KeyStroke): Boolean = {
        self ! StopRepl
        false
      }
    }


    case object CancelExecution extends Command {

      override val inState: State = AwaitingExecution
      override val priority       = 2

      override def matchesKey: PartialFunction[KeyStroke, Boolean] = {
        case KeyStroke(_, 'c', ctrlDown@true, _, _) => true
      }

      override def apply(keyStroke: KeyStroke): Boolean = {
        replProgram ! StopExecution
        state = Normal
        false
      }
    }

    case object NewCharacter extends Command {

      override val inState: State = Normal
      override val priority       = 3

      override def matchesKey: PartialFunction[KeyStroke, Boolean] = {
        case KeyStroke(_, c, _, _, _) if c != 0 => true
      }

      override def apply(keyStroke: KeyStroke): Boolean = {
        input += keyStroke.char
        true
      }
    }

    private def largeMovement(keyStroke: KeyStroke): Boolean = keyStroke.altDown

    override protected lazy val All: List[Command] = Enumeration.instancesOf[Command].sortBy(_.priority)
  }

}
