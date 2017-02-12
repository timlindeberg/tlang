package tcompiler.lexer

import java.io.File

import tcompiler.error.{ErrorLevel, Errors}
import tcompiler.imports.ImportMap
import tcompiler.lexer.Tokens.BAD
import tcompiler.utils.Positioned

/**
  * Created by Tim Lindeberg on 5/13/2016.
  */
trait LexerErrors extends Errors {

  override val ErrorLetters = "L"
  override var importMap    = new ImportMap(ctx)
  val file  : Option[File]
  var line  : Int
  var column: Int

  def error(errorCode: Int, msg: String, startPos: Positioned): Unit = {
    val file = startPos.file
    val pos = new Token(BAD).setPos(file, startPos.line, startPos.col, line, column)
    _error(errorCode, msg, pos)
  }

  protected def error(errorCode: Int, msg: String, colOffset: Int): Unit = {
    val pos = new Token(BAD).setPos(file, line, column, line, column + colOffset)
    _error(errorCode, msg, pos)
  }

  private def _error(errorCode: Int, msg: String, pos: Positioned) = {
    report(errorCode, msg, ErrorLevel.Error, pos)
  }

  //---------------------------------------------------------------------------------------
  //  Error messages
  //---------------------------------------------------------------------------------------

  // Missing 0

  protected def ErrorInvalidIdentifier(c: Char, length: Int): Unit =
    error(1, err"Invalid character in identifier: $c.", length)

  protected def ErrorUnclosedMultilineString(startPos: Positioned): Unit =
    error(2, err"Unclosed multiline string literal.", startPos)

  protected def ErrorEmptyCharLiteral(): Unit =
    error(3, err"Empty character literal.", 2)

  protected def ErrorInvalidEscapeSequence(length: Int): Unit =
    error(4, err"Invalid escape sequence.", length)

  protected def ErrorInvalidCharLiteral(length: Int): Unit =
    error(5, err"Invalid character literal.", length)

  protected def ErrorInvalidUnicode(length: Int): Unit =
    error(6, err"Invalid unicode escape sequence.", length)

  protected def ErrorUnclosedCharLiteral(length: Int): Unit =
    error(7, err"Unclosed character literal.", length)

  protected def ErrorUnclosedStringLiteral(length: Int): Unit =
    error(8, err"Unclosed string literal.", length)

  protected def ErrorNumberTooLargeForInt(length: Int): Unit =
    error(9, err"Number is too large to fit in an Int.", length)

  protected def ErrorNumberTooLargeForLong(length: Int): Unit =
    error(10, err"Number is too large to fit in a Long.", length)

  protected def ErrorInvalidNumber(length: Int): Unit =
    error(11, err"Invalid number.", length)

  protected def ErrorInvalidFloat(length: Int): Unit =
    error(12, err"Invalid floating point number.", length)

  protected def ErrorInvalidBinaryLiteral(length: Int): Unit =
    error(13, err"Invalid binary literal.", length)

  protected def ErrorInvalidHexadecimalLiteral(length: Int): Unit =
    error(14, err"Invalid hexadecimal literal.", length)
}
