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

  case class Resize(newWidth: Int) extends RenderingMessage
  case class DrawNewInput(inputBuffer: InputBuffer) extends RenderingMessage
  case class DrawCompileError(errors: Seq[CompilerMessage]) extends RenderingMessage
  case class DrawSuccess(output: String, truncate: Boolean) extends RenderingMessage
  case class DrawFailure(output: String, truncate: Boolean) extends RenderingMessage

  def props(formatter: Formatter, terminal: ReplTerminal, outputBox: OutputBox) =
    Props(new RenderingActor(formatter, terminal, outputBox))
  val name = "renderer"
}

class RenderingActor(
  formatter: Formatter,
  terminal: ReplTerminal,
  var outputBox: OutputBox) extends Actor {

  import RenderingActor._

  override def receive: Receive = {
    case msg: RenderingMessage =>
      msg match {
        case StartRepl           => terminal.newBox(outputBox.welcome())
        case Resize(newWidth)    => terminal.width = newWidth
        case DrawLoading         =>
          terminal.isCursorVisible = false
          outputBox = outputBox.nextLoadingState()
        case StopRepl            => outputBox = outputBox.exit()
        case DrawNewInput(input) =>
          outputBox = outputBox.newInput(input)
          terminal.updateCursor(input)
        case msg                 =>
          outputBox = msg match {
            case DrawCompileError(errors)            => outputBox.compileError(errors)
            case DrawSuccess(output, shouldTruncate) => outputBox.success(output, shouldTruncate)
            case DrawFailure(output, shouldTruncate) => outputBox.failure(output, shouldTruncate)
          }
          terminal.newBox(outputBox)
          outputBox = outputBox.clear()

      }
      terminal.updateBox(outputBox)
  }


}
