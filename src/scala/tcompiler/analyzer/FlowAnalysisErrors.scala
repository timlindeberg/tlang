package tcompiler.analyzer

import tcompiler.analyzer.Symbols.{ClassSymbol, _}
import tcompiler.analyzer.Types.Type
import tcompiler.ast.Trees.{Break, Tree, _}
import tcompiler.utils.{Errors, Positioned}

/**
  * Created by Tim Lindeberg on 5/13/2016.
  */
trait FlowAnalysisErrors extends Errors {

  override val ErrorPrefix = "F"

  def error(errorCode: Int, msg: String, tree: Positioned) =
    ctx.reporter.error(ErrorPrefix, errorCode, msg, tree, importMap)

  //---------------------------------------------------------------------------------------
  //  Error messages
  //---------------------------------------------------------------------------------------

  protected def ErrorAccessMightBeNull(v: VariableSymbol, pos: Positioned) =
    error(0, s"Cannot use nullable variable '$v' without first checking if it is 'null'.", pos)

  protected def ErrorAccessIsNull(v: VariableSymbol, pos: Positioned) =
    error(1, s"Cannot use nullable variable '$v' since it is known to be 'null'.", pos)

  protected def ErrorAccessNullableMethod(meth: String, pos: Positioned) =
    error(2, s"Cannot directly use result of method call '$meth' since it could be 'null'.", pos)

  protected def ErrorReassignmentToVal(value: String, pos: Positioned) =
    error(3, s"Cannot reassign value '$value'.", pos)

  //---------------------------------------------------------------------------------------
  //  Warnings
  //---------------------------------------------------------------------------------------

  protected def WarningDeadCode(startLine: Int, endLine: Int, pos: Positioned) = {
    val line = if(startLine == endLine) s"line '$startLine'" else s"lines '$startLine'-'$endLine'"
    warning(0, s"Code on $line is unreachable.", pos)
  }

  //---------------------------------------------------------------------------------------
  //  Private methods
  //---------------------------------------------------------------------------------------

}
