package tcompiler.ast

import tcompiler.lexer.{Token, TokenKind}
import tcompiler.utils.{Errors, NoPosition, Positioned}

/**
  * Created by Tim Lindeberg on 5/13/2016.
  */
trait ParserErrors extends Errors {

  override val ErrorPrefix = "P"

  private def error(errorCode: Int, msg: String, pos: Positioned = NoPosition) =
    ctx.reporter.error(ErrorPrefix, errorCode, msg, pos)

  //---------------------------------------------------------------------------------------
  //  Error messages
  //---------------------------------------------------------------------------------------

  protected def ErrorImplicitMethodOrOperator(pos: Positioned) =
    error(0, "Only constructors can be declared implicit.", pos)

  protected def ErrorStaticIndexingOperator(name: String, pos: Positioned) =
    error(1, s"Indexing operator '$name' cannot be declared static!", pos)

  protected def ErrorInvalidArrayDimension(size: Int, pos: Positioned) =
    error(2, s"Invalid array dimension: '$size', ${ASTBuilder.MaximumArraySize} is the maximum dimension of an array.", pos)

  protected def ErrorCantResolveImport(imp: String, pos: Positioned) =
    error(3, s"Cannot resolve import '$imp'.", pos)

  protected def ErrorConflictingImport(imp1: String, imp2: String, pos: Positioned) =
    error(4, s"Imports '$imp1' and '$imp2' are conflicting.", pos)

  //---------------------------------------------------------------------------------------
  //  Fatal messages
  //---------------------------------------------------------------------------------------

  protected def FatalExpectedIdAssignment(pos: Positioned) =
    fatal(1, "Expected identifier on left side of assignment.", pos)

  protected def FatalWrongToken(currentToken: Token, kind: TokenKind, more: TokenKind*): Nothing =
    FatalWrongToken((kind :: more.toList).map(k => s"'$k'").mkString(" or "), currentToken.toString, currentToken)

  protected def FatalWrongToken(expected: String, found: String, pos: Positioned): Nothing =
    fatal(2, s"Expected $expected, found: '$found'.", pos)

  protected def FatalUnexpectedToken(currentToken: Token) =
    fatal(3, s"Unexpected token: '$currentToken'", currentToken)

}
