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
  case object DrawLoading extends RendererMessage

  case class Resize(oldWidth: Int, newWidth: Int) extends RendererMessage
  case class DrawNewInput(inputBuffer: InputBuffer) extends RendererMessage
  case class DrawCompileError(errors: Seq[CompilerMessage]) extends RendererMessage
  case class DrawSuccess(output: String, truncate: Boolean) extends RendererMessage
  case class DrawFailure(output: String, truncate: Boolean) extends RendererMessage

  def props(formatter: Formatter, errorFormatter: MessageFormatter, maxOutputLines: Int, terminal: ReplTerminal) =
    Props(new Renderer(formatter, errorFormatter, maxOutputLines, terminal))
  val name = "renderer"
}

class Renderer(formatter: Formatter, errorFormatter: MessageFormatter, maxOutputLines: Int, terminal: ReplTerminal) extends Actor {

  import Renderer._

  private var inputBox: InputBox = newInputBox

  override def receive: Receive = {
    case msg: RendererMessage =>
      msg match {
        case StartRepl           => drawWelcomeBox()
        case DrawLoading         =>
          terminal.isCursorVisible = false
          inputBox.nextLoadingState()
        case StopRepl            => inputBox.exit()
        case DrawNewInput(input) =>
          terminal.isCursorVisible = true
          inputBox.newInput(input)
        case Resize(oldWidth, newWidth)              =>
          if(oldWidth > newWidth)
            terminal.clearPreviousDrawing()
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
    import formatter.formatting._

    val commands = List("help", "quit", "print").map(command => Magenta(s":$command"))
    val commandList = formatter.list(commands)
    val evaluateColor = Bold + Blue
    val exitColor = Bold + Red
    val tColor = Bold + Green

    val grid = formatter
      .grid
      .header(Bold("Welcome to the ") + tColor("T-REPL") + Bold("!"))
      .row()
      .content(
        s"""|Type in code to have it evaluated or type one of the following commands:
            |
            |$commandList
            |
            |Press ${ evaluateColor("CTRL") } + ${ evaluateColor("Space") } to evaluate the input and ${ exitColor("CTRL") } + ${exitColor("C") } to exit or type ${Magenta(":quit")}.
          """.stripMargin.trim
      )
    terminal.putBox(grid, resetStartPosition = false)
  }


}
