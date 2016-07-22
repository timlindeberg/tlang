package tcompiler.analyzer

import tcompiler.analyzer.Symbols.{ClassSymbol, _}
import tcompiler.analyzer.Types.Type
import tcompiler.ast.Trees.{Break, Tree, _}
import tcompiler.utils.{Errors, Positioned}

/**
  * Created by Tim Lindeberg on 5/13/2016.
  */
trait FlowAnalysisErrors extends Errors {

  override val ErrorPrefix = "N"

  def error(errorCode: Int, msg: String, tree: Positioned) =
    ctx.reporter.error(ErrorPrefix, errorCode, msg, tree, importMap)

  //---------------------------------------------------------------------------------------
  //  Error messages
  //---------------------------------------------------------------------------------------

  protected def ErrorInvalidNullableAccess(v: VariableID, pos: Positioned) =
    error(0, s"Cannot use nullable variable '$v' without first checking if it is 'null'.", pos)



  //---------------------------------------------------------------------------------------
  //  Private methods
  //---------------------------------------------------------------------------------------

}
