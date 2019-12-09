package tlang
package repl
package actors

import akka.actor.{Actor, Props}
import com.googlecode.lanterna.input.KeyType
import tlang.formatting.Formatter
import tlang.formatting.textformatters.StackTraceHighlighter
import tlang.repl.actors.EvaluationActor._
import tlang.repl.actors.RenderingActor.{DrawFailure, DrawLoading, DrawSuccess, RenderingMessage}
import tlang.repl.evaluation.{Evaluator, ReplState}
import tlang.repl.input.Input
import tlang.repl.terminal._
import tlang.utils.{Enumerable, Enumeration, Logging}

import scala.concurrent._

trait ExecutionState
case object AwaitingExecution extends ExecutionState
case object Normal extends ExecutionState

object ReplActor {
  case object Start
  case object Stop
  case object FinishedRendering
  case class SetState(state: ExecutionState)

  def props(
    replState: ReplState,
    stackTraceHighlighter: StackTraceHighlighter,
    evaluator: Evaluator,
    outputBox: OutputBox,
    terminal: ReplTerminal,
    input: Input,
    killProcessOnTerminate: Boolean
  )(
    implicit formatter: Formatter
  ) =
    Props(new ReplActor(replState, stackTraceHighlighter, evaluator, outputBox, terminal, input, killProcessOnTerminate))

  val name = "repl"
}

sealed abstract class Command(state: ExecutionState, val priority: Int) extends Product with Serializable {

  def keyAction: PartialFunction[Key, Boolean]

  def matches(currentState: ExecutionState, keyStroke: Key): Boolean = currentState == state && keyAction.isDefinedAt(keyStroke)
  def apply(keyStroke: Key): Boolean = keyAction.apply(keyStroke)
}

class ReplActor(
  replState: ReplState,
  stackTraceHighlighter: StackTraceHighlighter,
  evaluator: Evaluator,
  outputBox: OutputBox,
  terminal: ReplTerminal,
  input: Input,
  killProcessOnTerminate: Boolean
)(
  implicit formatter: Formatter,
) extends Actor with Logging {

  import ReplActor._
  import context.dispatcher
  import formatter._

  private val LoadingInterval = formatter.spinner.frameTime.length

  private val renderer = context.actorOf(
    RenderingActor.props(terminal, outputBox).withMailbox("rendererMailbox"),
    RenderingActor.name
  )

  private val replProgram = context.actorOf(
    EvaluationActor.props(replState, stackTraceHighlighter, evaluator),
    EvaluationActor.name
  )

  private var state: ExecutionState = Normal

  override def receive: Receive = {
    case SetState(state)        =>
      debug"Setting state $state"
      this.state = state
    case Start                  =>
      debug"Starting"
      replProgram ! Warmup
      renderer ! RenderingActor.StartRepl
      awaitInput()
    case Stop                   =>
      debug"Stopping"
      renderer ! RenderingActor.StopRepl
      terminal.close()
      input.saveToFile()
      val terminate = context.system.terminate()
      if (killProcessOnTerminate) terminate.map { sys.exit(0) } else terminate
    case msg: RenderingMessage  => renderer forward msg
    case msg: EvaluationMessage => replProgram forward msg
  }

  private def awaitInput(): Unit = {
    Future { terminal.readInput() } foreach {
      case OtherKey(KeyType.EOF, _, _, _) => self ! Stop
      case keyStroke                      =>
        debug"Got input $keyStroke"
        val shouldUpdateRenderer = Commands.find(_.matches(state, keyStroke)) match {
          case Some(command) => command(keyStroke)
          case None          => false
        }

        if (shouldUpdateRenderer)
          renderer ! RenderingActor.DrawNewInput(input.currentBuffer)
        awaitInput()
    }
  }

  private def setAwaitExecution(): Unit = {
    terminal.isCursorVisible = false
    state = AwaitingExecution
    debug"Setting state $AwaitingExecution"
    Future {
      //noinspection LoopVariableNotUpdated
      while (state == AwaitingExecution) {
        debug"Still awaiting execution"
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
          replProgram ! EvaluationActor.Evaluate(command)
          return
        }
        cmd.drop(1).toLowerCase match {
          case "help"          => renderer ! DrawSuccess("Help message", truncate = false)
          case "quit" | "exit" => self ! Stop
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
        case MouseClick(x, y, numClicks) =>
          numClicks match {
            case 1 => input.moveCursorTo(x, y, moveSecondary = true)
            case 2 => input.selectWord(x, y)
            case _ => input.selectLine(x, y)
          }
          true
        case MouseDrag(x, y)             =>
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
          self ! Stop
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

    override protected lazy val Values: List[Command] = Enumeration.instancesOf[Command].sortBy(_.priority)
  }

}
