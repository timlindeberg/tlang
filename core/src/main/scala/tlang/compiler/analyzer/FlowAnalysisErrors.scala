package tlang.compiler.analyzer

import tlang.compiler.ast.Trees.ExprTree
import tlang.compiler.error.{ErrorHandling, ErrorMessage, WarningMessage}
import tlang.utils.Positioned

trait FlowAnalysisErrors extends ErrorHandling {


  def report(error: ErrorMessage): Unit = reporter.report(error)

  //---------------------------------------------------------------------------------------
  //  Error messages
  //---------------------------------------------------------------------------------------

  val ErrorLetters = "F"
  abstract class FlowAnalysisError(code: Int, pos: Positioned) extends ErrorMessage(ErrorLetters, code, pos)
  abstract class FlowAnalysisWarning(code: Int, pos: Positioned) extends WarningMessage(ErrorLetters, code, pos)

  case class AccessMightBeNull(v: ExprTree, override val pos: Positioned) extends FlowAnalysisError(0, pos) {
    lazy val message = err"Cannot use nullable variable $v without first checking if it is null."
  }

  case class AccessIsNull(v: ExprTree, override val pos: Positioned) extends FlowAnalysisError(1, pos) {
    lazy val message = err"Cannot use nullable variable $v since it is known to be null."
  }

  case class AccessNullableMethod(meth: String, override val pos: Positioned) extends FlowAnalysisError(2, pos) {
    lazy val message = err"Cannot directly use result of method call $meth since it could be null."
  }

  case class DivideByZero(zeroExpr: ExprTree, override val pos: Positioned) extends FlowAnalysisError(3, pos) {
    lazy val message = err"Division by expression $zeroExpr is illegal since it is known to have the value ${ 0 }."
  }

  case class OutOfBounds(index: ExprTree, value: Int, size: Int, override val pos: Positioned) extends FlowAnalysisError(4, pos) {
    lazy val message: String = {
      val bounds = if (value < 0) s"$value < 0" else s"$value > $size"
      err"Indexing expression $index is out of bounds: $bounds."
    }
  }

  case class ReassignmentToVal(value: String, override val pos: Positioned) extends FlowAnalysisError(5, pos) {
    lazy val message = err"Cannot reassign value $value."
  }

  case class VariableNotInitialized(v: ExprTree, override val pos: Positioned) extends FlowAnalysisError(6, pos) {
    lazy val message = err"Cannot use variable $v since it may not have been initialized."
  }

  //---------------------------------------------------------------------------------------
  //  Warnings
  //---------------------------------------------------------------------------------------

  case class DeadCode(startLine: Int, endLine: Int, override val pos: Positioned) extends FlowAnalysisWarning(0, pos) {
    lazy val message: String = {
      val line = if (startLine == endLine) err"line $startLine" else err"lines $startLine - $endLine"
      err"Code on " + line + err" is unreachable."
    }

  }

  case class UnnecessaryCheck(value: ExprTree, knownNull: Boolean, override val pos: Positioned) extends FlowAnalysisWarning(1, pos) {
    lazy val message: String = {
      val known = if (knownNull) err"to be ${ "null" }" else err"not to be ${ "null" }"
      err"Check is unnecessary, $value is known " + known
    }
  }

  case class UnnecessaryElse(override val pos: Positioned) extends FlowAnalysisWarning(2, pos) {
    lazy val message = err"Else is unnecessary since validtests.code flow ends in then branch."
  }
}
