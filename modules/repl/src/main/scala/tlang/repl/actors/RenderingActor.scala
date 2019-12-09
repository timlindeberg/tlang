package tlang
package repl
package actors

import akka.actor.{Actor, Props}
import tlang.compiler.messages.CompilerMessage
import tlang.formatting.Formatter
import tlang.repl.input.InputBuffer
import tlang.repl.terminal.ReplTerminal
import tlang.utils.Logging

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

  def props(terminal: ReplTerminal, outputBox: OutputBox)(implicit formatter: Formatter) =
    Props(new RenderingActor(terminal, outputBox))
  val name = "renderer"
}

class RenderingActor(
  terminal: ReplTerminal,
  var outputBox: OutputBox
)(
  implicit formatter: Formatter
) extends Actor with Logging {

  import RenderingActor._

  var previousBox: OutputBox = _

  override def receive: Receive = {
    case msg: RenderingMessage => renderMessage(msg)
  }

  private def renderMessage(msg: RenderingMessage): Unit = {
    debug"Rendering $msg"
    msg match {
      case StartRepl           =>
        previousBox = outputBox.welcome()
        terminal.endBox(previousBox)
        outputBox = outputBox.clear()
      case Resize(newWidth)    =>
        terminal.width = newWidth
        terminal.updateBox(outputBox)
      case DrawLoading         =>
        terminal.isCursorVisible = false
        outputBox = outputBox.nextLoadingState()
      case StopRepl            =>
        previousBox = outputBox.exit()
        terminal.endBox(previousBox)
        outputBox = previousBox
      case DrawNewInput(input) =>
        outputBox = outputBox.newInput(input)
        terminal.updateCursor(input)
      case msg                 =>
        outputBox = msg match {
          case DrawCompileError(errors)            => outputBox.compileError(errors)
          case DrawSuccess(output, shouldTruncate) => outputBox.success(output, shouldTruncate)
          case DrawFailure(output, shouldTruncate) => outputBox.failure(output, shouldTruncate)
        }
        terminal.endBox(outputBox)
        outputBox = outputBox.clear()
    }
    if (previousBox != outputBox) {
      terminal.updateBox(outputBox)
      previousBox = outputBox
    }
  }
}
