package tlang

import java.lang.reflect.InvocationTargetException

import better.files.File
import tlang.compiler.{Context, Main}
import tlang.utils.Extensions._
import tlang.utils.{Logging, ProgramExecutor, StringSource}

object SimpleEvaluator {
  val ClassName = "Evaluation"
}

case class SimpleEvaluator(ctx: Context, programExecutor: ProgramExecutor) extends Logging {

  def apply(code: String): String = {
    val classFile = File(ctx.outDirs.head, SimpleEvaluator.ClassName + ".class")
    Main.Compiler.execute(ctx)(List(StringSource(code, SimpleEvaluator.ClassName)))
    val res = programExecutor(classFile)
    res.exception.ifDefined { e => throw new InvocationTargetException(e) }
    res.output
  }

}
