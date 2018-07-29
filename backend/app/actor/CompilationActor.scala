package actor

import akka.actor._
import play.api.Logger
import play.api.libs.json.{JsValue, Json, Writes}
import tlang._
import tlang.compiler.messages.CompilerMessage

import scala.util._

object CompilationActor {
  def props(out: ActorRef, evaluator: SafeEvaluator) = Props(new CompilationActor(out, evaluator))
}


object CompilationMessageType {
  val EVALUATE                = "EVALUATE"
  val CANCEL                  = "CANCEL"
  val TIMEOUT                 = "TIMEOUT"
  val SUCCESS                 = "SUCCESS"
  val NO_OUTPUT               = "NO_OUTPUT"
  val COMPILATION_ERROR       = "COMPILATION_ERROR"
  val EXECUTION_ERROR         = "EXECUTION_ERROR"
  val INTERNAL_COMPILER_ERROR = "INTERNAL_COMPILER_ERROR"
}


class CompilationActor(out: ActorRef, evaluator: SafeEvaluator) extends Actor {

  import context.dispatcher

  implicit val messageWriter: Writes[CompilerMessage] = (message: CompilerMessage) => {
    val pos = message.pos
    Json.obj(
      "start" -> Json.obj("line" -> pos.line, "col" -> pos.col),
      "end" -> Json.obj("line" -> pos.lineEnd, "col" -> pos.colEnd),
      "message" -> message.message
    )
  }

  def receive: PartialFunction[Any, Unit] = waiting

  import CompilationMessageType._

  private def waiting: PartialFunction[Any, Unit] = {
    case j: JsValue if hasType(j, EVALUATE) =>
      val code = (j \ "code").as[String]
      Logger.debug(s"Compiling code:\n $code")
      val Evaluation(result, cancel) = evaluator(code)
      result onComplete { onEvaluationComplete }
      context become compiling(cancel)
    case _                                  =>
  }

  private def compiling(cancelCompilation: () => Unit): PartialFunction[Any, Unit] = {
    case j: JsValue if hasType(j, CANCEL) =>
      cancelCompilation()
      Logger.debug("Canceling compilation")
      context become waiting
    case _                                =>
  }

  private def onEvaluationComplete(res: Try[EvaluationResult]): Unit = {
    context become waiting

    val message = res match {
      case Success(executionResult) => executionResult match {
        case ExecutionSuccessful(output)       => Json.obj("messageType" -> SUCCESS, "result" -> output)
        case NoOutput                          => Json.obj("messageType" -> NO_OUTPUT)
        case CompilationError(messages)        => Json.obj("messageType" -> COMPILATION_ERROR, "errors" -> messages)
        case Timeout                           => Json.obj("messageType" -> TIMEOUT, "timeout" -> evaluator.timeout.toSeconds)
        case Canceled                          => Json.obj("messageType" -> CANCEL)
        case ExecutionError(stackTrace, line)  => Json.obj("messageType" -> EXECUTION_ERROR, "error" -> stackTrace, "line" -> line.getOrElse(-1).asInstanceOf[Int])
        case InternalCompilerError(stackTrace) => Json.obj("messageType" -> INTERNAL_COMPILER_ERROR, "error" -> stackTrace)
      }
      case Failure(e)               =>
        e.printStackTrace()
        Json.obj("messageType" -> "SERVER_ERROR")
    }
    Logger.debug("Compilation complete. Result was: " + (message \ "messageType").get)
    out ! message
  }

  private def hasType(json: JsValue, messageType: String) = {
    (json \ "messageType").validate[String].asOpt.contains(messageType)
  }

}
