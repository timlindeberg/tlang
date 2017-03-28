package tlang.repl

import akka.actor.{Actor, Props}
import com.googlecode.lanterna.input.{KeyStroke, KeyType}
import tlang.compiler.Context
import tlang.repl.InputHandler.{InputHandlerMessage, NewInput, SaveToFile}
import tlang.repl.Renderer._
import tlang.repl.ReplProgram.ReplProgramMessage

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._

object Repl {
  case object Start
  case object Next
  case object Stop

  def props(ctx: Context, replTerminal: ReplTerminal, inputHistory: InputHistory) =
    Props(new Repl(ctx, replTerminal, inputHistory))

  val name = "repl"
}
class Repl(ctx: Context, terminal: ReplTerminal, inputHistory: InputHistory) extends Actor {

  import Repl._

  private val MaxOutputLines = 10

  private val renderer     = context.actorOf(Renderer.props(ctx.formatting, MaxOutputLines, terminal), Renderer.name)
  private val replProgram  = context.actorOf(ReplProgram.props(ctx, MaxOutputLines), ReplProgram.name)
  private val inputHandler = context.actorOf(InputHandler.props(ctx.formatting, inputHistory), InputHandler.name)

  override def receive: Receive = {
    case Start                    =>
      renderer ! Renderer.Start
      self ! Next
    case Stop                     =>
      inputHandler ! SaveToFile
      renderer ! Renderer.Stop
      context.system.terminate()
    case Next                     =>
      Future { terminal.readInput() } onSuccess {
        case key: KeyStroke if key.getKeyType == KeyType.EOF =>
          self ! Stop
        case key: KeyStroke                                  =>
          self ! Next
          inputHandler ! NewInput(key)
      }
    case msg: RendererMessage     => renderer forward msg
    case msg: ReplProgramMessage  => replProgram forward msg
    case msg: InputHandlerMessage => inputHandler forward msg
  }
}
