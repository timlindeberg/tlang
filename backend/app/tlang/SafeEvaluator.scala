package tlang

import java.io.ByteArrayOutputStream

import better.files.File
import tlang.compiler.ast.Trees.CompilationUnit
import tlang.compiler.messages.{CompilationException, CompilerMessage, MessageType}
import tlang.compiler.{Context, Main}
import tlang.utils.Extensions._
import tlang.utils.StringSource

import scala.concurrent.duration.Duration
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.sys.process._

object SafeEvaluator {
  val ClassName = "Evaluation"
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
    val classFile = File(ctx.outDirs.head, ClassName + ".class")
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
    execute(classFile)
  }

  private def execute(classFile: File)(implicit executionContext: ExecutionContext): (Future[ExecutionResult], () => Unit) = {

    val classPaths = ctx.allClassPaths.mkString(java.io.File.pathSeparator)
    val command = s"""java -cp "$classPaths" $ClassName 2>&1"""

    val byteOutput = new ByteArrayOutputStream()
    val logger = ProcessLogger(
      (out: String) => {
        byteOutput.write(out.getBytes)
        byteOutput.write('\n')
      },
      (err: String) => {
        byteOutput.write(err.getBytes)
        byteOutput.write('\n')
      }
    )

    val process = command.run(logger)

    val promise = Promise[ExecutionResult]()

    val cancel: () => Unit = () => {
      process.destroy()
      promise.success(Canceled)
    }

    promise completeWith Future { awaitResult(process, byteOutput) }

    (promise.future, cancel)
  }

  private def awaitResult(process: Process, byteOutput: ByteArrayOutputStream): ExecutionResult = {
    val startTime = System.currentTimeMillis()

    while (process.isAlive()) {
      val now = System.currentTimeMillis()
      if (now - startTime > timeout.toMillis) {
        process.destroy()
        return Timeout
      }
      Thread.sleep(100)
    }

    val exitCode = process.exitValue()
    val output = byteOutput.toString().trim
    if (exitCode != 0)
      return makeExecutionError(output)

    if (output.isEmpty) NoOutput else ExecutionSuccessful(output)
  }

  private def hasMainMethod(cus: List[CompilationUnit]) = {
    cus.exists { cu =>
      cu.classes.exists { clazz =>
        clazz.name == ClassName && clazz.methods.exists(_.getSymbol.isMainMethod)
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
}
