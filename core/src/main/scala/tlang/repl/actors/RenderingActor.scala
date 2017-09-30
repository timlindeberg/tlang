package tlang.repl.actors

import akka.actor.{Actor, Props}
import tlang.formatting.Formatter
import tlang.messages._
import tlang.repl.OutputBox
import tlang.repl.input.InputBuffer
import tlang.repl.terminal.ReplTerminal

object RenderingActor {

  trait RenderingMessage

  case object StartRepl extends RenderingMessage
  case object StopRepl extends RenderingMessage
  case object DrawLoading extends RenderingMessage

  case class Resize(oldWidth: Int, newWidth: Int) extends RenderingMessage
  case class DrawNewInput(inputBuffer: InputBuffer) extends RenderingMessage
  case class DrawCompileError(errors: Seq[CompilerMessage]) extends RenderingMessage
  case class DrawSuccess(output: String, truncate: Boolean) extends RenderingMessage
  case class DrawFailure(output: String, truncate: Boolean) extends RenderingMessage

  def props(formatter: Formatter, errorFormatter: MessageFormatter, terminal: ReplTerminal, maxOutputLines: Int) =
    Props(new RenderingActor(formatter, errorFormatter, terminal, maxOutputLines))
  val name = "renderer"
}

class RenderingActor(formatter: Formatter, errorFormatter: MessageFormatter, terminal: ReplTerminal, maxOutputLines: Int) extends Actor {

  import RenderingActor._

  private var outputBox: OutputBox = newOutputBox

  override def receive: Receive = {
    case msg: RenderingMessage =>
      msg match {
        case StartRepl                  => drawWelcomeBox()
        case DrawLoading                =>
          terminal.isCursorVisible = false
          outputBox.nextLoadingState()
        case StopRepl                   => outputBox.exit()
        case DrawNewInput(input)        =>
          terminal.isCursorVisible = true
          outputBox.newInput(input)
        case Resize(oldWidth, newWidth) =>
          if (oldWidth > newWidth)
            terminal.clearPreviousDrawing()
        case msg                        =>
          msg match {
            case DrawCompileError(errors)            => outputBox.compileError(errors)
            case DrawSuccess(output, shouldTruncate) => outputBox.success(output, shouldTruncate)
            case DrawFailure(output, shouldTruncate) => outputBox.failure(output, shouldTruncate)
          }
          outputBox.render()
          outputBox = newOutputBox

      }
      outputBox.render()
  }

  private def newOutputBox = new OutputBox(formatter, errorFormatter, terminal, maxOutputLines)

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
            |Press ${ evaluateColor("CTRL") } + ${ evaluateColor("Space") } to evaluate the input and ${ exitColor("CTRL") } + ${ exitColor("C") } to exit or type ${ Magenta(":quit") }.
          """.stripMargin.trim
      )
    terminal.putBox(grid, resetStartPosition = false)
  }


}
