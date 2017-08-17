package tlang.repl

import akka.actor.{Actor, Props}
import tlang.compiler.error._
import tlang.repl.input.InputBuffer
import tlang.utils.formatting.Formatter
import tlang.utils.formatting.grid.Grid

object Renderer {

  trait RendererMessage

  case object StartRepl extends RendererMessage
  case object StopRepl extends RendererMessage

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
          terminal.setCursorVisible(false)
          inputBox.nextLoadingState()
        case StopRepl            => inputBox.exit()
        case DrawNewInput(input) =>
          terminal.setCursorVisible(true)
          inputBox.newInput(input)
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
    val commandList = formatting.makeList(commands)
    val grid = Grid(formatter)
      .header(Bold("Welcome to the ") + SuccessColor("T-REPL") + Bold("!"))
      .row()
      .content(
        s"""|Type in validtests.code to have it evaluated or type one of the following commands:
            |
            |$commandList
            |
            |Press ${ Blue("CTRL") }+${ Blue("Space") } to evaluate the input.
          """.stripMargin.trim
      )
    terminal.put(grid.render() + "\n")
  }


}
