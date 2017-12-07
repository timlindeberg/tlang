package tlang.compiler

import tlang.compiler.ast.Trees.CompilationUnit
import tlang.compiler.messages.TemplateNameReplacer
import tlang.compiler.utils.DebugOutputFormatter
import tlang.formatting.{AlternativeSuggestor, ErrorStringContext, Formatting}
import tlang.utils.Extensions._
import tlang.utils.Logging

abstract class CompilerPhase[F, T] extends Logging {
  self =>

  val phaseName: String = getClass.simpleObjectName.toLowerCase

  def description(formatting: Formatting): String
  def printDebugOutput(output: List[T], debugOutputFormatter: DebugOutputFormatter): Unit
  protected def run(ctx: Context)(v: List[F]): List[T]

  def createErrorStringContext(ctx: Context, cu: CompilationUnit): ErrorStringContext = {
    ErrorStringContext(
      ctx.formatter,
      AlternativeSuggestor(),
      List[String => String](TemplateNameReplacer.apply, cu.imports.replaceNames)
    )
  }

  def andThen[G](thenn: CompilerPhase[T, G]): CompilerPhase[F, G] = new CompilerPhase[F, G] {
    def run(ctx: Context)(v: List[F]): List[G] = {
      val first = self.execute(ctx)(v)
      val second = thenn.execute(ctx)(first)
      second
    }

    override def description(formatting: Formatting): String = ""
    override def printDebugOutput(output: List[G], debugOutputFormatter: DebugOutputFormatter): Unit = {}

  }

  def execute(ctx: Context)(v: List[F]): List[T] = {
    if (Main.CompilerPhases.contains(this))
      info"Executing compiler stage $phaseName"

    val (output, time) = measureTime { run(ctx)(v) }
    if (Main.CompilerPhases.contains(this)) {
      if (!ctx.executionTimes.contains(phaseName))
        ctx.executionTimes += phaseName -> time

      if (phaseName in ctx.printCodePhase){
        printDebugOutput(output, ctx.debugOutputFormatter)
      }

    }
    ctx.reporter.terminateIfErrors()
    output
  }


}
