package tcompiler.analyzer

import tcompiler.ast.Trees._
import tcompiler.error.{ErrorLevel, Errors}
import tcompiler.utils.Positioned

/**
  * Created by Tim Lindeberg on 5/13/2016.
  */
trait FlowAnalysisErrors extends Errors {

  override val ErrorLetters = "F"

  def error(errorCode: Int, msg: String, pos: Positioned): Unit =
    report(errorCode, msg, ErrorLevel.Error, pos)

  //---------------------------------------------------------------------------------------
  //  Error messages
  //---------------------------------------------------------------------------------------

  protected def ErrorAccessMightBeNull(v: ExprTree, pos: Positioned): Unit =
    error(0, err"Cannot use nullable variable $v without first checking if it is null.", pos)

  protected def ErrorAccessIsNull(v: ExprTree, pos: Positioned): Unit =
    error(1, err"Cannot use nullable variable $v since it is known to be null.", pos)

  protected def ErrorAccessNullableMethod(meth: String, pos: Positioned): Unit =
    error(2, err"Cannot directly use result of method call $meth since it could be null.", pos)

  protected def ErrorDivideByZero(zeroExpr: ExprTree, pos: Positioned): Unit =
    error(3, err"Division by expression $zeroExpr is illegal since it is known to have the value ${0}.", pos)

  protected def ErrorOutOfBounds(index: ExprTree, value: Int, size: Int, pos: Positioned): Unit = {
    val bounds = if (value < 0) s"$value < 0" else s"$value > $size"
    error(4, err"Indexing expression $index is out of bounds: $bounds.", pos)
  }

  protected def ErrorReassignmentToVal(value: String, pos: Positioned): Unit =
    error(5, err"Cannot reassign value $value.", pos)

  protected def ErrorVariableNotInitialized(v: ExprTree, pos: Positioned): Unit =
    error(6, err"Cannot use variable $v since it may not have been initialized.", pos)

  //---------------------------------------------------------------------------------------
  //  Warnings
  //---------------------------------------------------------------------------------------

  protected def WarningDeadCode(startLine: Int, endLine: Int, pos: Positioned): Unit = {
    val line = if (startLine == endLine) err"line $startLine" else err"lines $startLine - $endLine"
    warning(0, err"Code on " + line + err"is unreachable.", pos)
  }

  protected def WarningUnnecessaryCheck(value: ExprTree, knownNull: Boolean, pos: Positioned): Unit = {
    val known = if (knownNull) err"to be ${"null"}" else err"not to be ${"null"}"
    warning(1, err"Check is unnecessary, $value is known " + known, pos)
  }

  protected def WarningUnnecessaryElse(pos: Positioned): Unit = {
    warning(2, err"Else is unnecessary since code flow ends in then branch.", pos)
  }
}
