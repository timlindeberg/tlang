package tlang.compiler.ast

import tlang.compiler.error.{ErrorLevel, Errors}
import tlang.compiler.imports.ImportMap
import tlang.compiler.lexer.{Token, TokenKind}
import tlang.compiler.utils.Positioned

/**
  * Created by Tim Lindeberg on 5/13/2016.
  */
trait ParserErrors extends Errors {

  override val ErrorLetters = "P"
  override var importMap    = new ImportMap(ctx)

  private def error(errorCode: Int, msg: String, pos: Positioned): Unit =
    report(errorCode, msg, ErrorLevel.Error, pos)

  //---------------------------------------------------------------------------------------
  //  Error messages
  //---------------------------------------------------------------------------------------

  protected def ErrorImplicitMethodOrOperator(pos: Positioned): Unit =
    error(0, err"Only constructors can be declared implicit.", pos)

  protected def ErrorStaticIndexingOperator(pos: Positioned): Unit =
    error(1, err"Indexing operators cannot be declared static.", pos)

  protected def ErrorInvalidArrayDimension(size: Int, pos: Positioned): Unit = {
    val maxArraySize = ASTBuilder.MaximumArraySize
    error(2, err"Invalid array dimension: $size, $maxArraySize is the maximum dimension of an array.", pos)
  }

  //---------------------------------------------------------------------------------------
  //  Fatal messages
  //---------------------------------------------------------------------------------------

  protected def FatalExpectedIdAssignment(pos: Positioned): Nothing =
    fatal(1, err"Expected identifier or array access on left side of assignment.", pos)

  protected def FatalWrongToken(currentToken: Token, kind: TokenKind, more: TokenKind*): Nothing = {
    val l = (kind :: more.toList).map(k => err"$k")
    val expected = l.size match {
      case 1 => l.head
      case 2 => l.head + err" or " + l.tail.mkString(err", ")
      case _ => l.dropRight(1).mkString(", ") + err" or " + l.last
    }
    FatalWrongToken(expected, currentToken.toString, currentToken)
  }

  protected def FatalWrongToken(expected: String, found: String, pos: Positioned): Nothing =
    fatal(2, err"Expected " + expected + err", found: $found.", pos)

  protected def FatalUnexpectedToken(currentToken: Token): Nothing =
    fatal(3, err"Unexpected token: $currentToken", currentToken)

}
