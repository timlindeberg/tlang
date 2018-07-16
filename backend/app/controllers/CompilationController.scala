package controllers

import actor.CompilationActor
import akka.actor.ActorSystem
import akka.stream.Materializer
import javax.inject._
import play.api.libs.json.JsValue
import play.api.libs.streams.ActorFlow
import play.api.mvc._
import tlang.SafeEvaluator
import tlang.compiler.Context
import tlang.compiler.imports.ClassPath
import tlang.compiler.messages.{CompilerMessages, DefaultReporter}
import tlang.compiler.output.JSONOutputHandler
import tlang.formatting.{Formatter, SimpleFormatting}

import scala.concurrent.duration.Duration

@Singleton
class CompilationController @Inject()(cc: ControllerComponents)(implicit system: ActorSystem, mat: Materializer) extends AbstractController(cc) {

  private val ctx = Context(
    reporter = DefaultReporter(CompilerMessages(maxErrors = 5)),
    formatter = Formatter(SimpleFormatting),
    output = JSONOutputHandler(),
    classPath = ClassPath.Default
  )

  private val evaluator = SafeEvaluator(ctx, Duration("10s"))

  def socket: WebSocket = WebSocket.accept[JsValue, JsValue] { request =>
    ActorFlow.actorRef { out =>
      CompilationActor.props(out, evaluator)
    }
  }
}
