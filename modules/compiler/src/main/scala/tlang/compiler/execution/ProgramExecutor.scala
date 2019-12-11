package tlang
package compiler
package execution

import tlang.compiler.argument.{ExecTimeoutFlag, JSONFlag}
import tlang.compiler.ast.Trees.CompilationUnit
import tlang.compiler.output.ExecutionResultOutput
import tlang.formatting.grid.Width.Fixed
import tlang.formatting.grid.{CenteredContent, Column, OverflowHandling}
import tlang.formatting.textformatters.{StackTraceHighlighter, SyntaxHighlighter}
import tlang.utils._

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, CancellationException, TimeoutException}
import scala.util.{Failure, Success, Try}

case class ProgramExecutor(ctx: Context, interruptionHandler: InterruptionHandler)
  (implicit syntaxHighlighter: SyntaxHighlighter, stackTraceHighlighter: StackTraceHighlighter) {

  import ctx.{formatter, options}

  private var cancelExecution: Option[CancellableFuture.CancelFunction] = None

  def apply(cus: Seq[CompilationUnit]): Unit = {

    val cusWithMainMethods = cus.filter(_.classes.exists(_.methods.exists(_.isMain)))
    val sources = cusWithMainMethods map { _.source.get }
    if (options(JSONFlag)) {
      val mainMethodExecutor = DefaultMainMethodExecutor(ctx.allClassPaths)
      val results = sources map { mainMethodExecutor(_) }

      ctx.output += ExecutionResultOutput(sources zip results)
    } else {
      sources foreach executeStreamingProgram
    }
  }

  private def executeStreamingProgram(source: Source): Unit = {
    import formatter._

    val boxLines = grid
      .header(Bold("Executing program"))
      .content(source.description)
      .row(Column(width = Fixed(6)), Column)
      .contents("", "")
      .render()
      .lines
      .toList

    val mainMethodExecutor = StreamingMainMethodExecutor(ctx.allClassPaths, printExecLine)
    boxLines.take(4).foreach(println)

    import scala.concurrent.ExecutionContext.Implicits.global

    val execution = mainMethodExecutor.executeAsync(source)
    awaitExecution(execution, source, boxLines.last)
  }

  private def awaitExecution(execution: CancellableFuture[ExecutionResult], source: Source, endOfBox: String): Unit = {
    val interruptHandlerId = interruptionHandler.setHandler(onInterrupt _)

    cancelExecution = Some(execution.cancel)
    val timeout = options(ExecTimeoutFlag)
    val result = Try(Await.result(execution.future, timeout))
    cancelExecution ifDefined { cancel => cancel() }
    println(endOfBox)
    result match {
      case Success(ExecutionResult(_, _, Some(exception))) => printStackTrace(source, exception)
      case Failure(_: TimeoutException)                    => printTimeout(timeout)
      case Failure(_: CancellationException)               => printCancelation()
      case _                                               =>
    }
    cancelExecution = None

    interruptionHandler.removeHandler(interruptHandlerId)
  }

  private def onInterrupt(): Unit = cancelExecution ifDefined { cancel => cancel() }

  private def printExecLine(line: String, lineNumber: Int): Unit = {
    import formatter._

    val s = grid
      .row(Column(width = Fixed(6), overflowHandling = OverflowHandling.Truncate), Column)
      .content(Magenta(lineNumber), syntaxHighlighter(line))
      .removeTop()
      .removeBottom()
      .render()
    print(s)
  }

  private def printStackTrace(source: Source, exception: Throwable): Unit = {
    import formatter._

    val stackTrace = stackTraceHighlighter.stackTraceFromFile(source.mainName, exception)
    grid
      .header((Red + Bold) ("There was an exception"))
      .row()
      .content(stackTrace)
      .print()
  }

  private def printCancelation(): Unit = {
    import formatter._
    grid
      .row()
      .content(CenteredContent(Red("Canceled execution")))
      .print()
  }

  private def printTimeout(timeout: Duration): Unit = {
    import formatter._
    grid
      .row()
      .content(CenteredContent(Red("Execution timed out after ") + (Bold + Red) (timeout.toMillis) + Red(" ms.")))
      .print()
  }
}
