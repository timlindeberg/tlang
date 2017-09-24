package tlang.repl

import akka.actor.{Actor, Props}
import tlang.formatting.Formatter
import tlang.messages._
import tlang.repl.input.InputBuffer
import tlang.repl.terminal.ReplTerminal

object Renderer {

  trait RendererMessage

  case object StartRepl extends RendererMessage
  case object StopRepl extends RendererMessage
  case object Redraw extends RendererMessage

  case object DrawLoading extends RendererMessage
  case class DrawNewInput(inputBuffer: InputBuffer) extends RendererMessage
  case class DrawCompileError(errors: Seq[CompilerMessage]) extends RendererMessage
  case class DrawSuccess(output: String, truncate: Boolean) extends RendererMessage
  case class DrawFailure(output: String, truncate: Boolean) extends RendererMessage

  def props(formatter: Formatter, errorFormatter: MessageFormatter, maxOutputLines: Int, terminal: ReplTerminal) =
    Props(new Renderer(formatter, errorFormatter, maxOutputLines, terminal))
  val name = "renderer"
}

class Renderer(formatter: Formatter, errorFormatter: MessageFormatter, maxOutputLines: Int, terminal: ReplTerminal) extends Actor {

  private val formatting = formatter.formatting

  import Renderer._
  import formatting._

  private val SuccessColor = Bold + Green
  private val InputColor   = Bold + Magenta

  private var inputBox: InputBox = _


  override def receive: Receive = {
    case msg: RendererMessage =>
      msg match {
        case StartRepl           =>
          drawWelcomeBox()
          inputBox = newInputBox
        case DrawLoading         =>
          terminal.isCursorVisible = false
          inputBox.nextLoadingState()
        case StopRepl            => inputBox.exit()
        case DrawNewInput(input) =>
          terminal.isCursorVisible = true
          inputBox.newInput(input)
        case Redraw              =>
        case msg                 =>
          msg match {
            case DrawCompileError(errors)            => inputBox.compileError(errors)
            case DrawSuccess(output, shouldTruncate) => inputBox.success(output, shouldTruncate)
            case DrawFailure(output, shouldTruncate) => inputBox.failure(output, shouldTruncate)
          }
          inputBox.render()
          inputBox = newInputBox

      }
      inputBox.render()
  }

  private def newInputBox = new InputBox(formatter, errorFormatter, maxOutputLines, terminal)

  private def drawWelcomeBox(): Unit = {
    val commands = List("help", "quit", "print").map(command => Magenta(s":$command"))
    val commandList = formatter.list(commands)
    val keyColor = Bold + Blue
    val grid = formatter
      .grid
      .header(Bold("Welcome to the ") + SuccessColor("T-REPL") + Bold("!"))
      .row()
      .content(
        s"""|Type in code to have it evaluated or type one of the following commands:
            |
            |$commandList
            |
            |Press ${ keyColor("CTRL") } + ${ keyColor("Space") } to evaluate the input.
          """.stripMargin.trim
      )
    terminal.putBox(grid, resetStartPosition = false)
  }


}
