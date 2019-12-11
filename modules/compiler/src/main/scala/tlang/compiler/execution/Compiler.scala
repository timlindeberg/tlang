package tlang
package compiler
package execution

import cafebabe.CodegenerationStackTrace
import tlang.compiler.analyzer.{Flowing, Naming, Typing}
import tlang.compiler.ast.Parsing
import tlang.compiler.ast.Trees.CompilationUnit
import tlang.compiler.code.CodeGeneration
import tlang.compiler.lexer.Lexing
import tlang.compiler.lowering.Lowering
import tlang.compiler.messages.{CompilationException, MessageType}
import tlang.compiler.modification.Templating
import tlang.compiler.output.ErrorMessageOutput
import tlang.formatting.textformatters.SyntaxHighlighter
import tlang.options.argument.MessageContextFlag
import tlang.utils.Source

object Compiler {

  val FrontEnd: CompilerPhase[Source, CompilationUnit] =
    Lexing andThen
      Parsing andThen
      Templating andThen
      Naming andThen
      Typing andThen
      Flowing

  val GenerateCode: CompilerPhase[CompilationUnit, CodegenerationStackTrace] =
    Lowering andThen CodeGeneration

  val Phases: Seq[CompilerPhase[_, _]] = List(
    Lexing,
    Parsing,
    Templating,
    Naming,
    Typing,
    Flowing,
    Lowering,
    CodeGeneration
  )
}

case class Compiler(ctx: Context)(implicit syntaxHighlighter: SyntaxHighlighter) {

  import Compiler._
  import ctx.{formatter, options}

  def apply(sources: List[Source]): Seq[CompilationUnit] = {
    val CUs = runFrontend(sources)
    GenerateCode.execute(ctx)(CUs)
    val messages = ctx.reporter.messages
    ctx.output += ErrorMessageOutput(messages, options(MessageContextFlag), List(MessageType.Warning))
    CUs
  }

  private def runFrontend(sources: List[Source]): List[CompilationUnit] = {
    try {
      FrontEnd.execute(ctx)(sources)
    } catch {
      case e: CompilationException =>
        ctx.output += ErrorMessageOutput(e.messages, options(MessageContextFlag))
        throw ExitException(1)
    }
  }
}
