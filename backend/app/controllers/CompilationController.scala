package controllers

import actor.CompilationActor
import akka.actor.ActorSystem
import akka.stream.Materializer
import better.files.File
import javax.inject._
import play.api.libs.streams.ActorFlow
import play.api.mvc.WebSocket.MessageFlowTransformer
import play.api.mvc._
import rapture.json.Json
import rapture.json.jsonBackends.play._
import tlang.SimpleEvaluator
import tlang.compiler.Context
import tlang.compiler.ast.PrettyPrinter
import tlang.compiler.code.TreeBuilder
import tlang.compiler.imports.{ClassPath, Imports}
import tlang.compiler.messages.{CompilerMessages, DefaultReporter, MessageFormatter}
import tlang.compiler.utils.DebugOutputFormatter
import tlang.formatting.textformatters.TabReplacer
import tlang.formatting.{ErrorStringContext, Formatter, SimpleFormatting}
import tlang.repl.evaluation.{Extractor, ReplState, SaveAndPrintTransformer}
import tlang.utils.ProgramExecutor

@Singleton
class CompilationController @Inject()(cc: ControllerComponents)(implicit system: ActorSystem, mat: Materializer) extends AbstractController(cc) {

  private val formatting = SimpleFormatting
  private val formatter  = Formatter(formatting)

  private val errorFormatter = MessageFormatter(formatter, TabReplacer(2), 3)

  private val tempDir = File.newTemporaryDirectory("repl")

  private val ctx = createContext(errorFormatter, tempDir)

  private val prettyPrinter      = PrettyPrinter(formatting)
  private val errorStringContext = ErrorStringContext(formatter)

  private val programExecutor = ProgramExecutor(ctx.allClassPaths)

  private val replState            = ReplState(prettyPrinter, Imports(ctx, errorStringContext))
  private val extractor            = Extractor(formatter, replState)
  private val statementTransformer = SaveAndPrintTransformer(TreeBuilder(), replState)
  private val evaluator            = SimpleEvaluator(ctx, programExecutor)

  implicit val messageFlowTransformer = MessageFlowTransformer.stringMessageFlowTransformer.map(
    (in: String) => Json.parse(in),
    (out: Json) => out.toBareString
  )

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

  def socket: WebSocket = WebSocket.accept[Json, Json] { request =>
    ActorFlow.actorRef { out =>
      CompilationActor.props(out, evaluator)
    }
  }
}
