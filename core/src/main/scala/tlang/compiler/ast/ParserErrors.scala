package tlang.compiler.ast

import tlang.compiler.error.{Error, ErrorHandling, Fatal}
import tlang.compiler.imports.ImportMap
import tlang.compiler.lexer.{Token, TokenKind}
import tlang.utils.Positioned

/**
  * Created by Tim Lindeberg on 5/13/2016.
  */
trait ParserErrors extends ErrorHandling {

  override val importMap = ImportMap(ctx)

  protected def lastToken: Token

  protected def report(error: Error): Unit = ctx.reporter.report(error)

  val ErrorLetters = "P"
  abstract class ParserFatal(code: Int, pos: Positioned) extends Fatal(ErrorLetters, code, pos)
  abstract class ParserError(code: Int, pos: Positioned) extends Error(ErrorLetters, code, pos)

  //---------------------------------------------------------------------------------------
  //  Error messages
  //---------------------------------------------------------------------------------------


  case class ImplicitMethodOrOperator(override val pos: Positioned) extends ParserError(0, pos) {
    lazy val message = err"Only constructors can be declared implicit."
  }


  case class StaticIndexingOperator(override val pos: Positioned) extends ParserError(1, pos) {
    lazy val message = err"Indexing operators cannot be declared static."
  }


  case class InvalidArrayDimension(size: Int, override val pos: Positioned) extends ParserError(2, pos) {
    lazy val message: String = {
      val maxArraySize = ASTBuilder.MaximumArraySize
      err"Invalid array dimension: $size, $maxArraySize is the maximum dimension of an array."
    }
  }

  //---------------------------------------------------------------------------------------
  //  Fatal messages
  //---------------------------------------------------------------------------------------


  case class ExpectedIdAssignment(override val pos: Positioned) extends ParserFatal(1, pos) {
    lazy val message = err"Expected identifier or array access on left side of assignment."
  }

  case class WrongToken(currentToken: Token, kind: TokenKind, more: TokenKind*)
    extends ParserFatal(2, restOf(currentToken)) {

    lazy val message: String = {
      val l = (kind :: more.toList).map(k => err"$k")
      val expected = l.size match {
        case 1 => l.head
        case 2 => l.head + err" or " + l.tail.mkString(err", ")
        case _ => l.dropRight(1).mkString(", ") + err" or " + l.last
      }
      val found = currentToken.toString
      err"Expected " + expected + err", found: $found."
    }

  }


  case class UnexpectedToken(currentToken: Token) extends ParserFatal(3, restOf(currentToken)) {
    lazy val message = err"Unexpected token: $currentToken"
  }

  private def restOf(currentToken: Token) = new Positioned {setPos(currentToken, lastToken)}


}
