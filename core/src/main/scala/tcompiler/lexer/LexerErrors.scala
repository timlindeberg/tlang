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
  val file  : File
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

  protected def ErrorInvalidCharacter(c: Char): Unit =
    error(0, s"Invalid character: '$c'.", 1)

  protected def ErrorInvalidIdentifier(c: Char, length: Int): Unit =
    error(1, s"Invalid character in identifier: '$c'.", length)

  protected def ErrorUnclosedMultilineString(startPos: Positioned): Unit =
    error(2, "Unclosed multiline string literal.", startPos)

  protected def ErrorEmptyCharLiteral(): Unit =
    error(3, "Empty character literal.", 2)

  protected def ErrorInvalidEscapeSequence(length: Int): Unit =
    error(4, "Invalid escape sequence.", length)

  protected def ErrorInvalidCharLiteral(length: Int): Unit =
    error(5, "Invalid character literal.", length)

  protected def ErrorInvalidUnicode(length: Int): Unit =
    error(6, "Invalid unicode escape sequence.", length)

  protected def ErrorUnclosedCharLiteral(startPos: Positioned): Unit =
    error(7, "Unclosed character literal.", startPos)

  protected def ErrorUnclosedStringLiteral(startPos: Positioned): Unit =
    error(8, "Unclosed string literal.", startPos)

  protected def ErrorNumberTooLargeForInt(length: Int): Unit =
    error(9, "Number is too large to fit in an Int.", length)

  protected def ErrorNumberTooLargeForLong(length: Int): Unit =
    error(10, "Number is too large to fit in a Long.", length)

  protected def ErrorInvalidNumber(length: Int, rest: List[Char]): Unit =
    error(11, "Invalid number.", getLength(length, rest))

  protected def ErrorInvalidFloat(length: Int, rest: List[Char]): Unit =
    error(12, "Invalid floating point number.", getLength(length, rest))

  protected def ErrorInvalidBinaryLiteral(length: Int, rest: List[Char]): Unit =
    error(13, "Invalid binary literal.", getLength(length, rest))

  protected def ErrorInvalidHexadecimalLiteral(length: Int, rest: List[Char]): Unit =
    error(14, "Invalid hexadecimal literal.", getLength(length, rest))

  private def getLength(length: Int, rest: List[Char]) = length + rest.indexWhere(_.isWhitespace) + 1

}
