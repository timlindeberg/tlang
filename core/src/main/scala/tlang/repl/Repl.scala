package tlang.repl

import akka.actor.{Actor, Props}
import akka.util.Timeout
import com.googlecode.lanterna.input.{KeyStroke, KeyType}
import tlang.compiler.Context
import tlang.repl.InputHandler.{InputHandlerMessage, NewInput, SaveToFile}
import tlang.repl.Renderer._
import tlang.repl.ReplProgram.ReplProgramMessage

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._
import scala.concurrent.duration._

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
    case Next                     =>
      Future { terminal.readInput() } onSuccess {
        case key: KeyStroke if key.getKeyType == KeyType.EOF =>
          self ! Stop
        case key: KeyStroke                                  =>
          self ! Next
          inputHandler ! NewInput(key)
      }
    case Stop                     =>
      implicit val timeout = Timeout(5 seconds)
      inputHandler ! SaveToFile
      renderer ! Renderer.Stop
      context.system.terminate()
    case msg: RendererMessage     =>
      //println("forwarded to renderer: " + msg)
      renderer forward msg
    case msg: ReplProgramMessage  =>
      //println("forwarded to replProgram: " + msg)
      replProgram forward msg
    case msg: InputHandlerMessage =>
      //println("forwarded to inputHandler: " + msg)
      inputHandler forward msg
  }
}
