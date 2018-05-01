package actor

import java.lang.reflect.InvocationTargetException
import java.util.concurrent.{CancellationException, TimeoutException}

import akka.actor._
import play.api.libs.json.{JsValue, Json, Writes}
import tlang.SimpleEvaluator
import tlang.compiler.messages.{CompilationException, CompilerMessage, MessageType}
import tlang.repl.evaluation.Evaluator.ClassName
import tlang.utils.CancellableFuture
import tlang.utils.Extensions.{NL, _}
import play.api.Logger

import scala.util._

object CompilationActor {
  def props(out: ActorRef, evaluator: SimpleEvaluator) = Props(new CompilationActor(out, evaluator))
}


object CompilationMessageType {
  val EVALUATE                = "EVALUATE"
  val CANCEL                  = "CANCEL"
  val TIMEOUT                 = "TIMEOUT"
  val SUCCESS                 = "SUCCESS"
  val COMPILATION_ERROR       = "COMPILATION_ERROR"
  val EXECUTION_ERROR         = "EXECUTION_ERROR"
  val INTERNAL_COMPILER_ERROR = "INTERNAL_COMPILER_ERROR"
}


class CompilationActor(out: ActorRef, evaluator: SimpleEvaluator) extends Actor {

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
      Logger.debug("Compiling code:\n" + code)
      val (f, cancel) = CancellableFuture { evaluator(code) }
      f onComplete { onCompilationComplete }
      context become compiling(cancel)
    case _                                  => println("WATING: doing nothing")
  }

  private def compiling(cancelCompilation: () => Boolean): PartialFunction[Any, Unit] = {
    case j: JsValue if hasType(j, CANCEL) =>
      cancelCompilation()
      Logger.debug("Canceling compilation")
      context become waiting
    case _                                => println("COMPILING: doing nothing")
  }

  private def onCompilationComplete(res: Try[String]): Unit = {
    context become waiting

    val message = res match {
      case Success(res) => Json.obj("messageType" -> SUCCESS, "result" -> res)
      case Failure(e)   =>
        e match {
          case e: CompilationException      => Json.obj("messageType" -> COMPILATION_ERROR, "errors" -> e.messages(MessageType.Error))
          case _: TimeoutException          => Json.obj("messageType" -> TIMEOUT)
          case _: CancellationException     => Json.obj("messageType" -> CANCEL)
          case e: InvocationTargetException => Json.obj("messageType" -> EXECUTION_ERROR, "error" -> formatStackTrace(e))
          case e                            => Json.obj("messageType" -> INTERNAL_COMPILER_ERROR, "error" -> e.stackTrace)
        }
    }
    Logger.debug("Compilation complete. Result was: " + (message \ "messageType").get)
    out ! message
  }

  private def hasType(json: JsValue, CompilationMessageType: String) = {
    (json \ "messageType").validate[String].isSuccess
  }

  private def formatStackTrace(e: Throwable): String = {
    val stackTrace = e.getCause.stackTrace
    // Remove internal parts of the stacktrace
    stackTrace.split("at " + ClassName).head + s"at $ClassName.main(Unknown Source)$NL"
  }
}
