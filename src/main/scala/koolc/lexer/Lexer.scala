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
      '/' -> DIV)

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

    private def getIdentifierOrKeyword(chars: List[Char]): (Token, List[Char]) = {
      def getIdentifierOrKeyword(chars: List[Char], s: String): (Token, List[Char]) =
        chars match {
          case (c :: r) if singleCharTokens.contains(c) || c.isWhitespace =>
            if (keyWords.contains(s)) {
              (createToken(keyWords(s), s.length), chars)
            } else {
              (createToken(new ID(s), s.length), chars)
            }
          case (c :: r) => getIdentifierOrKeyword(r, s + c)
        }
      getIdentifierOrKeyword(chars.tail, chars.head.toString)
    }

    private def getStringLiteral(chars: List[Char]): (Token, List[Char]) = {
      def getStringIdentifier(chars: List[Char], s: String): (Token, List[Char]) = chars match {
        case ('"' :: r)  => (createToken(new STRLIT(s), s.length + 2), r)
        case ('\n' :: r) => getStringIdentifier(r, s)
        case (c :: r)    => getStringIdentifier(r, s + c)
        case Nil         => (createToken(BAD, s.length), Nil)
      }
      getStringIdentifier(chars.tail, chars.head.toString)
    }

    private def getIntLiteral(chars: List[Char]): (Token, List[Char]) = {
      def getIntLiteral(chars: List[Char], s: String): (Token, List[Char]) = chars match {
        case (c :: r) if c.isDigit => getIntLiteral(r, s + c)
        case (c :: r)              => (createToken(new INTLIT(s.toInt), s.length), chars)
      }
      getIntLiteral(chars.tail, chars.head.toString)
    }

    private def createToken(kind: TokenKind, tokenLength: Int): Token = {
      var token = new Token(kind).setPos(file, Position.encode(line, column))
      column += tokenLength
      token
    }

    private def createToken(token: Token, tokenLength: Int): Token = {
      token.setPos(file, Position.encode(line, column))
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

    private def skipBlock(chars: List[Char]): List[Char] = {
      def skip(chars: List[Char]): List[Char] = chars match {
        case '*' :: '/' :: r =>
          column += 2
          r
        case '\n' :: r =>
          line += 1
          column = 1
          skip(r)
        case _ :: r =>
          column += 1
          skip(r)
        case Nil => Nil
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
        case '=' :: '=' :: r                          => readTokens(r, createToken(EQUALS, 2) :: tokens)
        case '=' :: r                                 => readTokens(r, createToken(EQSIGN, 1) :: tokens)
        case '/' :: '/' :: r                          => readTokens(skipLine(r), tokens)
        case '/' :: '*' :: r                          => readTokens(skipBlock(r), tokens)
        case '|' :: '|' :: r                          => readTokens(r, createToken(OR, 2) :: tokens)
        case '&' :: '&' :: r                          => readTokens(r, createToken(AND, 2) :: tokens)
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
        case _ :: r => (createToken(BAD, 1) :: tokens)
      }
      readTokens(chars, List[Token]()).reverse
    }

    def tokenize(): Iterator[Token] = readTokens(Source.fromFile(file).buffered.toList).iterator
  }

  def run(ctx: Context)(f: File): Iterator[Token] = {
    import ctx.reporter._

    new Tokenizer(f).tokenize
  }
}
