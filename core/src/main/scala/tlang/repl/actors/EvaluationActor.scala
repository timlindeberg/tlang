package tlang.repl.actors

import java.lang.reflect.InvocationTargetException

import akka.actor.{Actor, Props}
import tlang.formatting.Formatter
import tlang.messages.{CompilationException, MessageType}
import tlang.repl.actors.ReplActor.SetState
import tlang.repl.evaluation.{Evaluator, ReplState}
import tlang.utils.CancellableFuture
import tlang.utils.Extensions._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{CancellationException, TimeoutException}
import scala.util.{Failure, Success}


object EvaluationActor {

  trait EvaluationMessage

  case class Evaluate(command: String) extends EvaluationMessage
  case object Warmup extends EvaluationMessage
  case object PrettyPrint extends EvaluationMessage
  case object StopExecution extends EvaluationMessage

  def props(state: ReplState, evaluator: Evaluator, formatter: Formatter) =
    Props(new EvaluationActor(state, evaluator, formatter))

  val name = "replProgram"
}

class EvaluationActor(state: ReplState, evaluator: Evaluator, formatter: Formatter) extends Actor {

  import EvaluationActor._
  import Evaluator.ClassName

  private val formatting = formatter.formatting

  import formatting._

  private val WarmupProgram   = "val theAnswerToLifeInTheUniverseAndEverything: Int = 21 * 2"
  private val FailureColor    = Bold + Red
  private val NoCancel        = () => false
  private var cancelExecution = NoCancel

  private def parent = context.parent


  override def receive: Receive = {
    case Warmup            => evaluator(WarmupProgram)
    case Evaluate(command) => execute(command)
    case StopExecution     => cancelExecution()
    case PrettyPrint       => prettyPrint()
  }

  private def execute(command: String): Unit = {
    val (f, cancel) = CancellableFuture { evaluator(command) }
    cancelExecution = cancel
    f onComplete { res =>
      cancelExecution = NoCancel
      val renderMessage = res match {
        case Success(res) => RenderingActor.DrawSuccess(res, truncate = true)
        case Failure(e)   =>
          e match {
            case e: CompilationException      => RenderingActor.DrawCompileError(e.messages(MessageType.Error))
            case _: TimeoutException          => RenderingActor.DrawFailure(FailureColor("Execution timed out."), truncate = true)
            case _: CancellationException     => RenderingActor.DrawFailure(FailureColor("Execution cancelled."), truncate = true)
            case e: InvocationTargetException => RenderingActor.DrawFailure(formatStackTrace(e), truncate = true)
            case e                            =>
              val err = FailureColor("Internal compiler error:" + NL) + formatter.highlightStackTrace(e)
              RenderingActor.DrawFailure(err, truncate = true)
          }
      }
      // In case we got an exception and lalala
      state.clearStatements()
      parent ! SetState(Normal)
      parent ! renderMessage
    }
  }


  private def prettyPrint(): Unit = parent ! RenderingActor.DrawSuccess(state.prettyPrinted, truncate = false)


  private def formatStackTrace(e: Throwable) = {
    val stackTrace = e.getCause.stackTrace
    // Remove internal parts of the stacktrace
    val trimmed = stackTrace.split("at " + ClassName).head + s"at $ClassName.main(Unknown Source)$NL"
    formatter.highlightStackTrace(trimmed).rightTrimWhiteSpaces
  }


}
