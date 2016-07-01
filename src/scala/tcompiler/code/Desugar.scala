package tcompiler.code

import tcompiler.ast.Trees.CompilationUnit
import tcompiler.utils.{Context, Pipeline}

/**
  * Created by Tim Lindeberg on 7/1/2016.
  */
object Desugar extends Pipeline[List[CompilationUnit], List[CompilationUnit]] {

  override def run(ctx: Context)(cus: List[CompilationUnit]): List[CompilationUnit] = {
    cus
  }

}
