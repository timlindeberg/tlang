package tlang
package compiler
package ast

import tlang.compiler.lexer.Tokens.{DEDENT, INDENT}
import tlang.compiler.lexer.{Token, TokenKind}
import tlang.compiler.messages.{ErrorHandling, ErrorMessage}
import tlang.utils.Positioned

trait ParsingErrors extends ErrorHandling {

  protected def report(error: ErrorMessage): Unit = reporter.report(error)

  import errorStringContext._

  val ErrorLetters = "P"
  abstract class ParserError(code: Int, pos: Positioned) extends ErrorMessage(ErrorLetters, code, pos)

  //---------------------------------------------------------------------------------------
  //  Error messages
  //---------------------------------------------------------------------------------------

  case class WrongToken(currentToken: Token, expected: Seq[TokenKind], override val pos: Positioned) extends ParserError(0, pos) {

    lazy val message: String = {
      val expectedStrings = expected.map(k => err"$k")
      val found = currentToken.toString
      if (expected.isEmpty) {
        err"Unexpected token: $found"
      } else {
        val expectedMessage = expectedStrings.size match {
          case 1 => expectedStrings.head
          case 2 => expectedStrings.head + err" or " + expectedStrings.tail.mkString(err", ")
          case _ => expectedStrings.dropRight(1).mkString(", ") + err" or " + expectedStrings.last
        }
        err"Expected " + expectedMessage + err", found: $found." + indentWarning(expected)
      }
    }

    private def indentWarning(expected: Seq[TokenKind]) =
      if (expected.exists(_ in List(INDENT, DEDENT)))
        " Make sure you're using tabs and not spaces for indentation."
      else
        ""
  }

  case class ImplicitMethodOrOperator(override val pos: Positioned) extends ParserError(1, pos) {
    lazy val message = err"Only constructors can be declared implicit."
  }

  case class StaticIndexingOperator(override val pos: Positioned) extends ParserError(2, pos) {
    lazy val message = err"Indexing operators cannot be declared static."
  }

  case class InvalidArrayDimension(size: Int, override val pos: Positioned) extends ParserError(3, pos) {
    lazy val message: String = {
      val maxArraySize = Parser.MaximumArraySize
      err"Invalid array dimension: $size, $maxArraySize is the maximum dimension of an array."
    }
  }

  case class MainMethodAlreadyDefined(mainMethod: Positioned, override val pos: Positioned) extends ParserError(4, pos) {
    lazy val message: String = {
      err"Cannot have free statements since a main method is already defined at line ${ mainMethod.line }."
    }
  }

  case class FileClassAlreadyDefined(name: String, fileClass: Positioned, override val pos: Positioned) extends ParserError(5, pos) {
    lazy val message: String = {
      err"Cannot have free statements or methods in file $name.t since a class with the name the $name is already defined at line ${ fileClass.line }. Consider creating a main method inside $name instead."
    }
  }
}
