package tlang
package compiler
package ast

import tlang.compiler.lexer.Tokens.{DEDENT, INDENT}
import tlang.compiler.lexer.{Token, TokenKind}
import tlang.compiler.messages.{ErrorHandling, ErrorMessage, FatalMessage}
import tlang.utils.Positioned

trait ParsingErrors extends ErrorHandling {

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

  case class MainMethodAlreadyDefined(mainMethod: Positioned, override val pos: Positioned) extends ParserError(3, pos) {
    lazy val message: String = {
      err"Cannot have free statements since a main method is already defined at line ${ mainMethod.line }."
    }
  }

  case class FileClassAlreadyDefined(name: String, fileClass: Positioned, override val pos: Positioned) extends ParserError(4, pos) {
    lazy val message: String = {
      err"Cannot have free statements or methods in file $name.t since a class with the name the $name is already defined at line ${ fileClass.line }. Consider creating a main method inside $name instead."
    }
  }

  //---------------------------------------------------------------------------------------
  //  Fatal messages
  //---------------------------------------------------------------------------------------

  case class ExpectedIdAssignment(override val pos: Positioned) extends ParserFatal(1, pos) {
    lazy val message = err"Expected identifier or array access on left side of assignment."
  }

  case class WrongToken(currentToken: Token, lastToken: Token, kind: TokenKind, more: Seq[TokenKind])
    extends ParserFatal(2, restOf(currentToken, lastToken)) {

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
      if (expected.exists(_ in List(INDENT, DEDENT)))
        " Make sure you're using tabs and not spaces for indentation."
      else
        ""
  }

  case class UnexpectedToken(currentToken: Token, lastToken: Token)
    extends ParserFatal(3, restOf(currentToken, lastToken)) {
    lazy val message = err"Unexpected token: $currentToken"
  }

  private def restOf(currentToken: Token, lastToken: Token) = new Positioned {setPos(currentToken, lastToken) }
}
