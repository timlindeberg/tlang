package tlang

import java.io.ByteArrayOutputStream
import java.util.UUID

import better.files.File
import play.api.Logger
import tlang.compiler.ast.Trees.CompilationUnit
import tlang.compiler.messages.{CompilationException, CompilerMessage, MessageType}
import tlang.compiler.{Context, Main}
import tlang.utils.Extensions._
import tlang.utils.StringSource

import scala.concurrent.duration.Duration
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.language.postfixOps
import scala.sys.process._

object SafeEvaluator {
  val ClassName = "Evaluation"

  val ExecScript = "docker/execute_in_docker.sh"

}

trait EvaluationResult

case class ExecutionSuccessful(output: String) extends EvaluationResult
case class ExecutionError(stackTrace: String, line: Option[Int]) extends EvaluationResult
case class CompilationError(errorMessages: List[CompilerMessage]) extends EvaluationResult
case class InternalCompilerError(stackTrace: String) extends EvaluationResult
case object Timeout extends EvaluationResult
case object Canceled extends EvaluationResult
case object NoOutput extends EvaluationResult

case class Evaluation(result: Future[EvaluationResult], cancel: () => Unit)

case class SafeEvaluator(ctx: Context, timeout: Duration) {

  import SafeEvaluator._

  def apply(code: String)(implicit executionContext: ExecutionContext): Evaluation = {
    val source = List(StringSource(code, ClassName))
    val identity = () => {}

    val outDir = File.newTemporaryDirectory()

    val ctx = this.ctx.copy(outDirs = Set(outDir))

    try {
      val cus = Main.FrontEnd.execute(ctx)(source)
      if (!hasMainMethod(cus))
        return Evaluation(Future(NoOutput), identity)
      Main.GenerateCode.execute(ctx)(cus)
    } catch {
      case e: CompilationException =>
        val executionResult = CompilationError(e.messages(MessageType.Error))
        return Evaluation(Future(executionResult), identity)
      case e: Exception            =>
        val executionResult = InternalCompilerError(e.stackTrace)
        return Evaluation(Future(executionResult), identity)
    }

    execute(outDir)
  }

  private def execute(outDir: File)(implicit executionContext: ExecutionContext): Evaluation = {
    val id = UUID.randomUUID()

    val command = Seq(ExecScript, outDir.pathAsString, ClassName, id.toString)
    val output = new ByteArrayOutputStream()

    def writeLine(s: String): Unit = {
      output.write(s.getBytes)
      output.write('\n')
    }

    Logger.debug(s"Executing command: ${ command.mkString(" ") }")

    val process = Process(command).run(ProcessLogger(fout = writeLine, ferr = writeLine))
    val promise = Promise[EvaluationResult]()

    val cancel: () => Unit = () => {
      Logger.debug("Execution canceled")
      cancelExecution(id)
      promise.success(Canceled)
    }

    promise completeWith Future { awaitResult(process, id, output) }

    Evaluation(promise.future, cancel)
  }

  private def awaitResult(process: Process, id: UUID, output: ByteArrayOutputStream): EvaluationResult = {
    val startTime = System.currentTimeMillis()

    while (process.isAlive()) {
      val now = System.currentTimeMillis()
      if (now - startTime > timeout.toMillis) {
        Logger.debug("Execution timed out")
        cancelExecution(id)
        return Timeout
      }
      Thread.sleep(100)
    }

    val exitCode = process.exitValue
    val outputString = output.toString.trim
    if (exitCode != 0)
      return makeExecutionError(outputString)

    if (outputString.isEmpty) NoOutput else ExecutionSuccessful(outputString)
  }

  private def hasMainMethod(cus: List[CompilationUnit]) = {
    cus.exists { cu =>
      cu.classes.exists { clazz =>
        clazz.name == ClassName && clazz.methods.exists { _.getSymbol.isMainMethod }
      }
    }
  }

  private def makeExecutionError(output: String) = {
    val line = """\..+?\(.+?:(\d+)\)""".r.findFirstMatchIn(output).map(_.group(1).toInt)
    val message = output
      .replaceAll("Exception in thread \"main\" ", "")
      .replaceAll(ClassName + "[.:]?", "")
    ExecutionError(message, line)
  }

  private def cancelExecution(id: UUID): Unit = s"docker kill $id" !!

}
