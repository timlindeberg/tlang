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

  def matches(currentState: State, keyStroke: Key): Boolean = currentState == state && matchesKey.isDefinedAt(keyStroke)
  def matchesKey: PartialFunction[Key, Boolean]
  def apply(keyStroke: Key): Boolean
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
      terminal.disableMouseReporting()
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
      override def matchesKey: PartialFunction[Key, Boolean] = {
        case CharacterKey((' ' | 128), Ctrl(true), _, _) => true
      }

      override def apply(keyStroke: Key): Boolean = {
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

    case object Undo extends Command(Normal, 1) {

      override def matchesKey: PartialFunction[Key, Boolean] = {
        case CharacterKey('z', Ctrl(true), _, _) => true
      }

      override def apply(keyStroke: Key): Boolean = input.undo()
    }

    case object Redo extends Command(Normal, 1) {

      override def matchesKey: PartialFunction[Key, Boolean] = {
        case CharacterKey('z', Ctrl(true), Alt(true), _) => true
      }

      override def apply(keyStroke: Key): Boolean = input.redo()
    }

    case object Remove extends Command(Normal, 1) {

      override def matchesKey: PartialFunction[Key, Boolean] = {
        case OtherKey(KeyType.Backspace, _, _, _) => true
      }

      override def apply(keyStroke: Key): Boolean = {
        if (keyStroke.isAltDown) input.removeToLeftWord() else input.removeSelected()
        true
      }
    }

    case object MoveCursorWithArrows extends Command(Normal, 1) {

      override def matchesKey: PartialFunction[Key, Boolean] = {
        case _: ArrowKey => true
      }

      override def apply(keyStroke: Key): Boolean = {
        val key = keyStroke.asInstanceOf[ArrowKey]
        val isShiftDown = keyStroke.isShiftDown
        val isAltDown = keyStroke.isAltDown

        key.direction match {
          case Direction.Left  => input.left(isAltDown, isShiftDown)
          case Direction.Right => input.right(isAltDown, isShiftDown)
          case Direction.Up    => input.up(isShiftDown)
          case Direction.Down  => input.down(isShiftDown)
        }
        true
      }
    }

    case object MouseMovement extends Command(Normal, 1) {
      override def matchesKey: PartialFunction[Key, Boolean] = {
        case _: MouseEvent => true
      }

      override def apply(keyStroke: Key): Boolean = {
        keyStroke match {
          case MouseDown(x, y) => input.moveCursorTo(x, y, moveSecondary = true)
          case MouseDrag(x, y) => input.moveCursorTo(x, y, moveSecondary = false)
          case MouseUp(_, _)   => // Do nothing for release
        }
        true
      }

    }

    case object Copy extends Command(Normal, 1) {

      override def matchesKey: PartialFunction[Key, Boolean] = {
        case CharacterKey('c', Ctrl(true), Alt(true), _) => true
      }

      override def apply(keyStroke: Key): Boolean = {
        input.copySelected()
        true
      }
    }

    case object Paste extends Command(Normal, 1) {

      override def matchesKey: PartialFunction[Key, Boolean] = {
        case CharacterKey('v', Ctrl(true), Alt(true), _) => true
      }

      override def apply(keyStroke: Key): Boolean = {
        input.paste()
        true
      }
    }

    case object Cut extends Command(Normal, 1) {

      override def matchesKey: PartialFunction[Key, Boolean] = {
        case CharacterKey('x', Ctrl(true), Alt(true), _) => true
      }

      override def apply(keyStroke: Key): Boolean = {
        input.cutSelected()
        true
      }
    }

    case object ExitProgram extends Command(Normal, 2) {

      override def matchesKey: PartialFunction[Key, Boolean] = {
        case CharacterKey('c', Ctrl(true), _, _) => true
      }

      override def apply(keyStroke: Key): Boolean = {
        self ! StopRepl
        false
      }
    }


    case object CancelExecution extends Command(AwaitingExecution, 2) {

      override def matchesKey: PartialFunction[Key, Boolean] = {
        case CharacterKey('c', Ctrl(true), _, _) => true
      }

      override def apply(keyStroke: Key): Boolean = {
        replProgram ! StopExecution
        state = Normal
        false
      }
    }

    case object NewCharacter extends Command(Normal, 3) {

      override def matchesKey: PartialFunction[Key, Boolean] = {
        case CharacterKey(_, _, _, _) => true
      }

      override def apply(keyStroke: Key): Boolean = {
        input += keyStroke.asInstanceOf[CharacterKey].char
        true
      }
    }

    override protected lazy val All: List[Command] = Enumeration.instancesOf[Command].sortBy(_.priority)
  }

}
