package tlang

import java.lang.reflect.InvocationTargetException

import better.files.File
import tlang.compiler.{Context, Main}
import tlang.utils.Extensions._
import tlang.utils.{ProgramExecutor, StringSource}

object SimpleEvaluator {
  val ClassName = "Evaluation"
}

case class SimpleEvaluator(ctx: Context, programExecutor: ProgramExecutor) {

  def apply(code: String): String = {
    val classFile = File(ctx.outDirs.head, SimpleEvaluator.ClassName + ".class")
    val source = List(StringSource(code, SimpleEvaluator.ClassName))
    Main.Compiler.execute(ctx)(source)
    val res = programExecutor(classFile)
    res.exception.ifDefined { e => throw new InvocationTargetException(e) }
    res.output
  }

}
