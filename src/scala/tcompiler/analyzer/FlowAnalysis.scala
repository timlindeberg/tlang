package tcompiler.analyzer

import tcompiler.ast.Trees.CompilationUnit
import tcompiler.imports.ImportMap
import tcompiler.utils.{Context, Pipeline}

/**
  * Created by timlindeberg on 22/07/16.
  */
class FlowAnalysis extends Pipeline[List[CompilationUnit], List[CompilationUnit]] with FlowAnalysisErrors {
  override def run(ctx: Context)(v: List[CompilationUnit]): List[CompilationUnit] = ???
  override var ctx      : Context     = _
  override var importMap: ImportMap = _
}
