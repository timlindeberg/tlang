package tlang.compiler

import tlang.compiler.ast.Trees.CompilationUnit
import tlang.compiler.messages.TemplateNameReplacer
import tlang.compiler.output.Output
import tlang.formatting._
import tlang.utils.Extensions._
import tlang.utils.JSON.Json
import tlang.utils.Logging

abstract class CompilerPhase[F, T] extends Logging {
  self =>

  val phaseName: String = getClass.simpleObjectName.toLowerCase

  def description(formatting: Formatting): String
  def debugOutput(output: List[T], formatter: Formatter): Output
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
    override def debugOutput(output: List[G], formatter: Formatter): Output = null

  }

  def execute(ctx: Context)(v: List[F]): List[T] = {
    if (Main.CompilerPhases.contains(this))
      info"Executing compiler stage $phaseName"

    val (output, time) = measureTime { run(ctx)(v) }
    if (Main.CompilerPhases.contains(this)) {
      ctx.executionTimes += phaseName -> time

      if (phaseName in ctx.printCodePhase)
        ctx.output += debugOutput(output, ctx.formatter)
    }
    ctx.reporter.terminateIfErrors()
    output
  }

  def json: Json = Json(
    "name" -> phaseName,
    "description" -> description(SimpleFormatting)
  )


}
