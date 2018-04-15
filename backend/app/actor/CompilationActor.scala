package actor

import java.lang.reflect.InvocationTargetException
import java.util.concurrent.CancellationException

import akka.actor._
import rapture.json._
import rapture.json.dictionaries.dynamic._
import rapture.json.jsonBackends.play._
import rapture.net._
import tlang.SimpleEvaluator
import tlang.compiler.messages.{CompilationException, MessageType}
import tlang.repl.evaluation.Evaluator.ClassName
import tlang.utils.CancellableFuture
import tlang.utils.Extensions.{NL, _}

import scala.util.Try

object CompilationActor {
  def props(out: ActorRef, evaluator: SimpleEvaluator) = Props(new CompilationActor(out, evaluator))
}

trait CompilationMessageType {
  def data: Option[String] = None
  def messageType: String

  def toJson: Json = data match {
    case Some(data) => json"""{ "messageType": $messageType, "data": $data }"""
    case None       => json"""{ "messageType": $messageType }"""
  }
}

abstract class CompilationMessage(override val messageType: String) extends CompilationMessageType
abstract class CompilationMessageWithData(override val messageType: String, result: String) extends CompilationMessageType {
  override def data: Option[String] = Some(result)
}

object CompilationMessageType {
  case object Evaluate extends CompilationMessage("EVALUATE")
  case object Cancel extends CompilationMessage("CANCEL")
  case object Timeout extends CompilationMessage("TIMEOUT")
  case class Success(result: String) extends CompilationMessageWithData("SUCCESS", result)
  case class CompilationError(error: String) extends CompilationMessageWithData("COMPILATION_ERROR", error)
  case class ExecutionError(error: String) extends CompilationMessageWithData("EXECUTION_ERROR", error)
  case class InternalCompilerError(error: String) extends CompilationMessageWithData("INTERNAL_COMPILER_ERROR", error)
}


class CompilationActor(out: ActorRef, evaluator: SimpleEvaluator) extends Actor {

  import context.dispatcher

  def receive: PartialFunction[Any, Unit] = waiting

  import CompilationMessageType._

  private def waiting: PartialFunction[Any, Unit] = {
    case j: Json if hasType(j, Evaluate) =>
      val code = j.code.as[String]
      println("Code: " + code)
      val (f, cancel) = CancellableFuture { evaluator(code) }
      f onComplete { onCompilationComplete }
      context become compiling(cancel)
    case _                               => println("WATING: doing nothing")
  }

  private def compiling(cancelCompilation: () => Boolean): PartialFunction[Any, Unit] = {
    case j: Json if hasType(j, Cancel) =>
      cancelCompilation()
      context become waiting
    case _                             => println("COMPILING: doing nothing")
  }

  private def onCompilationComplete(res: Try[String]): Unit = {
    val message = res match {
      case util.Success(res) => Success(res)
      case util.Failure(e)   =>
        e match {
          case e: CompilationException      => CompilationError(e.messages(MessageType.Error).toString)
          case _: TimeoutException          => Timeout
          case _: CancellationException     => Cancel
          case e: InvocationTargetException => ExecutionError(formatStackTrace(e))
          case e                            => InternalCompilerError(e.stackTrace)
        }
    }
    out ! message.toJson
    context become waiting
  }


  private def hasType(json: Json, compilationMessageType: CompilationMessageType) = {
    import rapture.core.modes.returnOption._
    val MessageType = compilationMessageType.messageType
    json.messageType.as[String] match {
      case Some(MessageType) => true
      case _                 => false
    }
  }

  private def formatStackTrace(e: Throwable): String = {
    val stackTrace = e.getCause.stackTrace
    // Remove internal parts of the stacktrace
    stackTrace.split("at " + ClassName).head + s"at $ClassName.main(Unknown Source)$NL"
  }
}
