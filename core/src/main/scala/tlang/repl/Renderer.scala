package tlang.repl

import akka.actor.{Actor, Props}
import tlang.compiler.error._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object Renderer {

  trait RendererMessage

  case object Start extends RendererMessage
  case object Stop extends RendererMessage
  case object Loading extends RendererMessage

  case class NewInput(inputBuffer: InputBuffer) extends RendererMessage
  case class CompileError(errors: List[ErrorMessage]) extends RendererMessage
  case class Success(output: String, truncate: Boolean) extends RendererMessage
  case class Failure(output: String, truncate: Boolean) extends RendererMessage

  def props(formatting: Formatting, maxOutputLines: Int, terminal: ReplTerminal) =
    Props(new Renderer(formatting, maxOutputLines, terminal))
  val name = "renderer"
}

class Renderer(formatting: Formatting, maxOutputLines: Int, terminal: ReplTerminal) extends Actor {

  import Renderer._
  import formatting._
  import formatting.colors._

  private val SuccessColor = Bold + Green
  private val InputColor   = Bold + Magenta

  private var isLoading          = false
  private var inputBox: InputBox = _


  override def receive: Receive = {
    case Loading              =>
      isLoading = true
      Future { Thread.sleep(spinner.frameTime.length) } onSuccess { case _ =>
        if (isLoading) {
          inputBox.nextLoadingState()
          inputBox.render()
          self ! Loading
        }
      }
    case msg: RendererMessage =>
      def newInputBox = new InputBox(formatting, maxOutputLines, terminal)
      isLoading = false
      msg match {
        case Start                     =>
          drawWelcomeBox()
          inputBox = newInputBox
        case Stop                      =>
          inputBox.exit()
        case NewInput(input)           =>
          inputBox.newInput(input)
        case CompileError(errors)      =>
          inputBox.compileError(errors)
          inputBox.render()
          inputBox = newInputBox
        case Success(output, truncate) =>
          inputBox.success(output, truncate)
          inputBox.render()
          inputBox = newInputBox
        case Failure(output, truncate) =>
          inputBox.failure(output, truncate)
          inputBox.render()
          inputBox = newInputBox
      }
      inputBox.render()
  }

  private def drawWelcomeBox(): Unit = {
    val header = Bold("Welcome to the ") + SuccessColor("T-REPL") + Bold("!")
    val description =
      s"""
         |Type in code to have it evaluated or type one of the following commands:
         |   ${ InputColor(":help") }
         |   ${ InputColor(":quit") }
         |   ${ InputColor(":print") }
     """.trim.stripMargin
    val box = makeBox(header, description :: Nil)
    terminal.put(box)
  }


}
