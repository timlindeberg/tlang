package controllers

import actor.CompilationActor
import akka.actor.ActorSystem
import akka.stream.Materializer
import better.files.File
import javax.inject._
import play.api.libs.json.JsValue
import play.api.libs.streams.ActorFlow
import play.api.mvc._
import tlang.SafeEvaluator
import tlang.compiler.Context
import tlang.compiler.imports.ClassPath
import tlang.compiler.messages.{CompilerMessages, DefaultReporter, MessageFormatter}
import tlang.compiler.utils.DebugOutputFormatter
import tlang.formatting.textformatters.TabReplacer
import tlang.formatting.{Formatter, SimpleFormatting}

import scala.concurrent.duration.Duration

@Singleton
class CompilationController @Inject()(cc: ControllerComponents)(implicit system: ActorSystem, mat: Materializer) extends AbstractController(cc) {

  private val formatting = SimpleFormatting
  private val formatter  = Formatter(formatting)

  private val errorFormatter = MessageFormatter(formatter, TabReplacer(2), 3)

  private val tempDir = File.newTemporaryDirectory("repl")

  private val ctx = createContext(errorFormatter, tempDir)

  private val evaluator = SafeEvaluator(ctx, Duration("2s"))

  private def createContext(errorFormatter: MessageFormatter, tempDir: File): Context = {
    val formatter = errorFormatter.formatter
    val classPath = ClassPath.Default + tempDir.pathAsString

    val errorMessages = CompilerMessages(formatter, errorFormatter, maxErrors = 5)
    val debugOutputFormatter = DebugOutputFormatter(formatter)
    Context(
      reporter = DefaultReporter(errorMessages),
      formatter = formatter,
      debugOutputFormatter = debugOutputFormatter,
      classPath = classPath,
      outDirs = Set(tempDir)
    )
  }

  def socket: WebSocket = WebSocket.accept[JsValue, JsValue] { request =>
    ActorFlow.actorRef { out =>
      CompilationActor.props(out, evaluator)
    }
  }
}
