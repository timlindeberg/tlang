package koolc
package lexer

import utils._
import scala.io.Source
import java.io.File
import scala.collection.mutable.ArrayBuffer
import scala.runtime.RichChar

object Lexer extends Pipeline[File, Iterator[Token]] {
  import Tokens._

  class Tokenizer(val file: File) {

    val singleCharTokens = Map(
      ':' -> COLON,
      ';' -> SEMICOLON,
      '.' -> DOT,
      ',' -> COMMA,
      '!' -> BANG,
      '(' -> LPAREN,
      ')' -> RPAREN,
      '[' -> LBRACKET,
      ']' -> RBRACKET,
      '{' -> LBRACE,
      '}' -> RBRACE,
      '+' -> PLUS,
      '-' -> MINUS,
      '*' -> TIMES,
      '<' -> LESSTHAN,
      '/' -> DIV,
      '=' -> EQSIGN)

    val keyWords = Map(
      "object" -> OBJECT,
      "class" -> CLASS,
      "def" -> DEF,
      "var" -> VAR,
      "Unit" -> UNIT,
      "main" -> MAIN,
      "String" -> STRING,
      "extends" -> EXTENDS,
      "Int" -> INT,
      "Bool" -> BOOLEAN,
      "while" -> WHILE,
      "if" -> IF,
      "else" -> ELSE,
      "return" -> RETURN,
      "length" -> LENGTH,
      "true" -> TRUE,
      "false" -> FALSE,
      "this" -> THIS,
      "new" -> NEW,
      "println" -> PRINTLN)

    var line = 1
    var column = 1

    private def createIdentifierOrKeyWord(s: String): Token = {
      if (keyWords.contains(s))
        createToken(keyWords(s), s.length)
      else
        createIdToken(s, s.length)
    }

    private def getIdentifierOrKeyword(chars: List[Char]): (Token, List[Char]) = {
      def getIdentifierOrKeyword(chars: List[Char], s: String): (Token, List[Char]) = {
        val end = (c: Char) => singleCharTokens.contains(c) || c.isWhitespace
        val validChar = (c: Char) => c.isLetter || c.isDigit || c == '_'
        chars match {
          case (c :: r) if validChar(c) => getIdentifierOrKeyword(r, s + c)
          case (c :: r) if end(c)       => (createIdentifierOrKeyWord(s), chars)
          case (c :: r)                 => (createToken(BAD, s.length), chars)
          case Nil                      => (createIdentifierOrKeyWord(s), chars)
        }
      }
      getIdentifierOrKeyword(chars.tail, chars.head.toString)
    }

    private def getStringLiteral(chars: List[Char]): (Token, List[Char]) = {
      def getStringIdentifier(chars: List[Char], s: String): (Token, List[Char]) = chars match {
        case ('"' :: r)  => (createToken(s, s.length + 2), r)
        case ('\n' :: r) => (createToken(BAD, s.length), chars)
        case (c :: r)    => getStringIdentifier(r, s + c)
        case Nil         => (createToken(BAD, s.length), Nil)
      }
      getStringIdentifier(chars, "")
    }

    private def getIntLiteral(chars: List[Char]): (Token, List[Char]) = {
      def getIntLiteral(chars: List[Char], s: String): (Token, List[Char]) = chars match {
        case (c :: r) if c.isDigit => getIntLiteral(r, s + c)
        case (c :: r)              => (createToken(s.toInt, s.length), chars)
      }
      getIntLiteral(chars, "")
    }

    private def createToken(kind: TokenKind, tokenLength: Int): Token = {
      var token = new Token(kind).setPos(file, Position.encode(line, column))
      column += tokenLength
      token
    }

    private def createToken(integer: Int, tokenLength: Int): Token = {
      var token = new INTLIT(integer).setPos(file, Position.encode(line, column))
      column += tokenLength
      token
    }

    private def createIdToken(string: String, tokenLength: Int): Token = {
      var token = new ID(string).setPos(file, Position.encode(line, column))
      column += tokenLength
      token
    }

    private def createToken(string: String, tokenLength: Int): Token = {
      var token = new STRLIT(string).setPos(file, Position.encode(line, column))
      column += tokenLength
      token
    }

    private def skipLine(chars: List[Char]): List[Char] = {
      def skip(chars: List[Char]): List[Char] =
        chars match {
          case '\n' :: r =>
            line += 1
            column = 1
            r
          case _ :: r =>
            column += 1
            skip(r)
          case Nil => Nil
        }
      column += 2
      skip(chars)
    }

    private def skipBlock(chars: List[Char]): (Option[Token], List[Char]) = {
      def skip(chars: List[Char]): (Option[Token], List[Char]) = chars match {
        case '*' :: '/' :: r =>
          column += 2
          (None, r)
        case '\n' :: r =>
          line += 1
          column = 1
          skip(r)
        case _ :: r =>
          column += 1
          skip(r)
        case Nil => (Some(createToken(BAD, 1)), Nil)
      }
      column += 2
      skip(chars)
    }

    def readTokens(chars: List[Char]): List[Token] = {
      def readTokens(chars: List[Char], tokens: List[Token]): List[Token] = chars match {
        case '\n' :: r =>
          column = 1
          line += 1
          readTokens(r, tokens)
        case (c :: r) if c.isWhitespace =>
          column += 1
          readTokens(r, tokens)
        case '/' :: '/' :: r => readTokens(skipLine(r), tokens)
        case '/' :: '*' :: r =>
          val (token, tail) = skipBlock(r)
          readTokens(tail, if (token.isDefined) token.get :: tokens else tokens)
        case '=' :: '=' :: r                          => readTokens(r, createToken(EQUALS, 2) :: tokens)
        case '|' :: '|' :: r                          => readTokens(r, createToken(OR, 2) :: tokens)
        case '&' :: '&' :: r                          => readTokens(r, createToken(AND, 2) :: tokens)
        case '0' :: r                                 => readTokens(r, createToken(0, 1) :: tokens)
        case (c :: r) if singleCharTokens.contains(c) => readTokens(r, createToken(singleCharTokens(c), 1) :: tokens)
        case (c :: r) if c.isLetter =>
          var (token, tail) = getIdentifierOrKeyword(chars)
          readTokens(tail, token :: tokens)
        case ('"' :: r) =>
          var (token, tail) = getStringLiteral(r)
          readTokens(tail, token :: tokens)
        case (c :: r) if c.isDigit =>
          var (token, tail) = getIntLiteral(chars)
          readTokens(tail, token :: tokens)
        case Nil    => (new Token(Tokens.EOF).setPos(file, Position.encode(line, column - 1)) :: tokens)
        case _ :: r => readTokens(r, (createToken(BAD, 1) :: tokens))
      }
      readTokens(chars, List[Token]()).reverse
    }

    def tokenize(chars: List[Char]): Iterator[Token] = readTokens(chars).iterator
  }

  def run(ctx: Context)(f: File): Iterator[Token] = {
    import ctx.reporter._

    new Tokenizer(f).tokenize(Source.fromFile(f).buffered.toList)
  }

  def run(chars: List[Char], f: File): Iterator[Token] = new Tokenizer(f).tokenize(chars)
}
