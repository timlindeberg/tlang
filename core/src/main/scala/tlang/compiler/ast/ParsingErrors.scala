package tlang.compiler.ast

import tlang.compiler.error.{ErrorHandling, ErrorMessage, FatalMessage}
import tlang.compiler.lexer.Tokens.{DEDENT, INDENT}
import tlang.compiler.lexer.{Token, TokenKind}
import tlang.utils.Positioned

trait ParsingErrors extends ErrorHandling {

  protected def lastToken: Token

  protected def report(error: ErrorMessage): Unit = reporter.report(error)


  import errorStringContext._

  val ErrorLetters = "P"
  abstract class ParserFatal(code: Int, pos: Positioned) extends FatalMessage(ErrorLetters, code, pos)
  abstract class ParserError(code: Int, pos: Positioned) extends ErrorMessage(ErrorLetters, code, pos)


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
      val maxArraySize = Parser.MaximumArraySize
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
      val expected = kind :: more.toList
      val expectedStrings = expected.map(k => err"$k")
      val expectedMessage = expectedStrings.size match {
        case 1 => expectedStrings.head
        case 2 => expectedStrings.head + err" or " + expectedStrings.tail.mkString(err", ")
        case _ => expectedStrings.dropRight(1).mkString(", ") + err" or " + expectedStrings.last
      }
      val found = currentToken.toString
      err"Expected " + expectedMessage + err", found: $found." + indentWarning(expected)
    }

    private def indentWarning(expected: List[TokenKind]) =
      if (expected.contains(INDENT) || expected.contains(DEDENT))
        " Make sure you're using tabs and not spaces for indentation."
      else
        ""

  }


  case class UnexpectedToken(currentToken: Token) extends ParserFatal(3, restOf(currentToken)) {
    lazy val message = err"Unexpected token: $currentToken"
  }

  private def restOf(currentToken: Token) = new Positioned {setPos(currentToken, lastToken) }


}
