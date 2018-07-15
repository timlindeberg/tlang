package tlang

import java.io.ByteArrayOutputStream
import java.util.UUID

import better.files.File
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

trait ExecutionResult

case class ExecutionSuccessful(output: String) extends ExecutionResult
case class ExecutionError(stackTrace: String, line: Option[Int]) extends ExecutionResult
case class CompilationError(errorMessages: List[CompilerMessage]) extends ExecutionResult
case class InternalCompilerError(stackTrace: String) extends ExecutionResult
case object Timeout extends ExecutionResult
case object Canceled extends ExecutionResult
case object NoOutput extends ExecutionResult

case class SafeEvaluator(ctx: Context, timeout: Duration) {

  import SafeEvaluator._

  def apply(code: String)(implicit executionContext: ExecutionContext): (Future[ExecutionResult], () => Unit) = {
    val source = List(StringSource(code, ClassName))
    val identity = () => {}

    try {
      val cus = Main.FrontEnd.execute(ctx)(source)
      if (!hasMainMethod(cus))
        return (Future(NoOutput), identity)
      Main.GenerateCode.execute(ctx)(cus)
    } catch {
      case e: CompilationException =>
        val executionResult = CompilationError(e.messages(MessageType.Error))
        return (Future(executionResult), identity)
      case e: Exception            =>
        val executionResult = InternalCompilerError(e.stackTrace)
        return (Future(executionResult), identity)
    }

    val classFile = File(ctx.outDirs.head, ClassName + ".class")
    execute(classFile)
  }

  private def execute(classFile: File)(implicit executionContext: ExecutionContext): (Future[ExecutionResult], () => Unit) = {
    val id = UUID.randomUUID()
    val command = Seq(ExecScript, classFile.pathAsString, id.toString)
    val output = new ByteArrayOutputStream()

    def writeLine(s: String): Unit = {
      output.write(s.getBytes)
      output.write('\n')
    }

    val process = Process(command).run(ProcessLogger(fout = writeLine, ferr = writeLine))
    val promise = Promise[ExecutionResult]()

    val cancel: () => Unit = () => {
      cancelExecution(id)
      promise.success(Canceled)
    }

    promise completeWith Future { awaitResult(process, id, output) }

    (promise.future, cancel)
  }

  private def awaitResult(process: Process, id: UUID, output: ByteArrayOutputStream): ExecutionResult = {
    val startTime = System.currentTimeMillis()

    while (process.isAlive()) {
      val now = System.currentTimeMillis()
      if (now - startTime > timeout.toMillis) {
        cancelExecution(id)
        return Timeout
      }
      Thread.sleep(100)
    }

    val exitCode = process.exitValue()
    val outputString = output.toString().trim
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
