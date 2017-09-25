package tlang.repl


import akka.actor.{Actor, Props}
import com.googlecode.lanterna.input.KeyType
import tlang.Context
import tlang.compiler.ast.PrettyPrinter
import tlang.messages.MessageFormatter
import tlang.repl.Renderer._
import tlang.repl.ReplProgram._
import tlang.repl.input.Input
import tlang.repl.terminal._
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

sealed abstract class Command(state: State, val priority: Int) extends Product with Serializable {

  def keyAction: PartialFunction[Key, Boolean]

  def matches(currentState: State, keyStroke: Key): Boolean = currentState == state && keyAction.isDefinedAt(keyStroke)
  def apply(keyStroke: Key): Boolean = keyAction.apply(keyStroke)
}

class Repl(
  ctx: Context,
  errorFormatter: MessageFormatter,
  prettyPrinter: PrettyPrinter,
  terminal: ReplTerminal,
  input: Input) extends Actor {

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
    Future { terminal.readInput() } foreach {
      case OtherKey(KeyType.EOF, _, _, _) => self ! StopRepl
      case keyStroke                      =>
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
    terminal.isCursorVisible = false
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

    case object Evaluate extends Command(Normal, 1) {
      override def keyAction: PartialFunction[Key, Boolean] = {
        case CharacterKey((' ' | 128), Ctrl(true), _, _) =>
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

    case object UndoRedo extends Command(Normal, 1) {

      override def keyAction: PartialFunction[Key, Boolean] = {
        case CharacterKey('_', Ctrl(true), _, Shift(true))  => input.redo()
        case CharacterKey('_', Ctrl(true), _, Shift(false)) => input.undo()
      }
    }

    case object Remove extends Command(Normal, 1) {

      override def keyAction: PartialFunction[Key, Boolean] = {
        case OtherKey(KeyType.Backspace, _, Alt(isAltDown), _) =>
          if (isAltDown) input.removeToLeftWord() else input.removeSelected()
          true
      }
    }

    case object MoveCursorWithArrows extends Command(Normal, 1) {

      override def keyAction: PartialFunction[Key, Boolean] = {
        case ArrowKey(direction, _, Alt(isAltDown), Shift(isShiftDown)) =>
          direction match {
            case Direction.Left  => input.left(isAltDown, isShiftDown)
            case Direction.Right => input.right(isAltDown, isShiftDown)
            case Direction.Up    => input.up(isShiftDown)
            case Direction.Down  => input.down(isShiftDown)
          }
          true
      }
    }

    case object MouseMovement extends Command(Normal, 1) {

      override def keyAction: PartialFunction[Key, Boolean] = {
        case MouseDown(x, y) =>
          input.moveCursorTo(x, y, moveSecondary = true)
          true
        case MouseDrag(x, y) =>
          input.moveCursorTo(x, y, moveSecondary = false)
          true
      }

    }

    case object MoveToStartOrEndOfLine extends Command(Normal, 1) {

      override def keyAction: PartialFunction[Key, Boolean] = {
        case CharacterKey('a', Ctrl(true), _, Shift(isShiftDown)) =>
          input.moveCursorToStartOfLine(isShiftDown)
          true
        case CharacterKey('e', Ctrl(true), _, Shift(isShiftDown)) =>
          input.moveCursorToEndOfLine(isShiftDown)
          true
      }

    }

    case object RemoveToStartOfLine extends Command(Normal, 1) {

      override def keyAction: PartialFunction[Key, Boolean] = {
        case CharacterKey('u', Ctrl(true), _, _) =>
          input.removeToStartOfLine()
          true
      }

    }

    case object CopyPasteCut extends Command(Normal, 1) {

      override def keyAction: PartialFunction[Key, Boolean] = {
        case CharacterKey('c', Ctrl(true), Alt(true), _) =>
          input.copySelected()
          true
        case CharacterKey('v', Ctrl(true), Alt(true), _) =>
          input.paste()
          true
        case CharacterKey('x', Ctrl(true), Alt(true), _) =>
          input.cutSelected()
          true
      }

    }

    case object ExitProgram extends Command(Normal, 2) {

      override def keyAction: PartialFunction[Key, Boolean] = {
        case CharacterKey('c', Ctrl(true), _, _) =>
          self ! StopRepl
          false
      }

    }


    case object CancelExecution extends Command(AwaitingExecution, 2) {

      override def keyAction: PartialFunction[Key, Boolean] = {
        case CharacterKey('c', Ctrl(true), _, _) =>
          replProgram ! StopExecution
          state = Normal
          false
      }

    }

    case object NewCharacter extends Command(Normal, 3) {

      override def keyAction: PartialFunction[Key, Boolean] = {
        case CharacterKey(char, _, _, _) =>
          input += char
          true
      }

    }

    override protected lazy val All: List[Command] = Enumeration.instancesOf[Command].sortBy(_.priority)
  }

}
