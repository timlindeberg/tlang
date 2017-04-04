package tlang.compiler.lexer

import tlang.compiler.error.{Error, ErrorHandling}
import tlang.compiler.imports.ImportMap
import tlang.compiler.lexer.Tokens.BAD
import tlang.utils.{Positioned, Source}

trait LexerErrors extends ErrorHandling {

  override val importMap = ImportMap(ctx)
  val source: Source
  var line  : Int
  var column: Int

  def report(error: Error): Unit = ctx.reporter.report(error)

  private def pos(colOffset: Int) = {
    new Token(BAD).setPos(source, line, column, line, column + colOffset)
  }

  private def pos(startPos: Positioned) = {
    new Token(BAD).setPos(source, startPos.line, startPos.col, line, column)
  }

  //---------------------------------------------------------------------------------------
  //  Error messages
  //---------------------------------------------------------------------------------------

  // Missing 0

  abstract class LexerError(code: Int, pos: Positioned) extends Error("L", code, pos)

  case class InvalidIdentifier(c: Char, length: Int) extends LexerError(1, pos(length)) {
    lazy val message = err"Invalid character in identifier: $c."
  }

  case class UnclosedMultilineString(startPos: Positioned) extends LexerError(2, pos(startPos)) {
    lazy val message = err"Unclosed multiline string literal."
  }

  case class EmptyCharLiteral() extends LexerError(3, pos(2)) {
    lazy val message = err"Empty character literal."
  }

  case class InvalidEscapeSequence(length: Int) extends LexerError(4, pos(length)) {
    lazy val message = err"Invalid escape sequence."
  }

  case class InvalidCharLiteral(length: Int) extends LexerError(5, pos(length)) {
    lazy val message = err"Invalid character literal."
  }

  case class InvalidUnicode(length: Int) extends LexerError(6, pos(length)) {
    lazy val message = err"Invalid unicode escape sequence."
  }

  case class UnclosedCharLiteral(length: Int) extends LexerError(7, pos(length)) {
    lazy val message = err"Unclosed character literal."
  }

  case class UnclosedStringLiteral(length: Int) extends LexerError(8, pos(length)) {
    lazy val message = err"Unclosed string literal."
  }

  case class NumberTooLargeForInt(length: Int) extends LexerError(9, pos(length)) {
    lazy val message = err"Number is too large to fit in an ${"Int"}."
  }

  case class NumberTooLargeForLong(length: Int) extends LexerError(10, pos(length)) {
    lazy val message = err"Number is too large to fit in a ${"Long"}."
  }

  case class InvalidNumber(length: Int) extends LexerError(11, pos(length)) {
    lazy val message = err"Invalid number."
  }

  case class InvalidFloat(length: Int) extends LexerError(12, pos(length)) {
    lazy val message = err"Invalid floating point number."
  }

  case class InvalidBinaryLiteral(length: Int) extends LexerError(13, pos(length)) {
    lazy val message = err"Invalid binary literal."
  }

  case class InvalidHexadecimalLiteral(length: Int) extends LexerError(14, pos(length)) {
    lazy val message = err"Invalid hexadecimal literal."
  }

}
