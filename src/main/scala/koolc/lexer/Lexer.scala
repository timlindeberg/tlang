package koolc
package lexer

import utils._
import scala.io.Source
import java.io.File
import scala.collection.mutable.ArrayBuffer
import scala.util.control.TailCalls.TailRec
import scala.annotation.tailrec

object Lexer extends Pipeline[File, Iterator[Token]] {
  import Tokens._

  class Tokenizer(private val file: File) {

    private val singleCharTokens = Map(
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
      '>' -> GREATERTHAN,
      '/' -> DIV,
      '=' -> EQSIGN,
      '?' -> QUESTIONMARK,
      '%' -> MODULO,
      '~' -> LOGICNOT,
      '&' -> LOGICAND,
      '|' -> LOGICOR,
      '^' -> LOGICXOR)

    private val keyWords = Map(
      "object" -> OBJECT,
      "class" -> CLASS,
      "def" -> DEF,
      "var" -> VAR,
      "main" -> MAIN,
      "String" -> STRING,
      "extends" -> EXTENDS,
      "Unit" -> UNIT,
      "Int" -> INT,
      "Bool" -> BOOLEAN,
      "while" -> WHILE,
      "for" -> FOR,
      "if" -> IF,
      "else" -> ELSE,
      "return" -> RETURN,
      "length" -> LENGTH,
      "true" -> TRUE,
      "false" -> FALSE,
      "this" -> THIS,
      "new" -> NEW,
      "println" -> PRINTLN)

    private var line = 1
    private var column = 1

    private def createIdentifierOrKeyWord(s: String): Token = {
      if (keyWords.contains(s))
        createToken(keyWords(s), s.length)
      else
        createIdToken(s, s.length)
    }

    private def getIdentifierOrKeyword(chars: List[Char]): (Token, List[Char]) = {
      def getIdentifierOrKeyword(chars: List[Char], s: String): (Token, List[Char]) = {
        val end = (c: Char) => singleCharTokens.contains(c) || c.isWhitespace || c == '&' || c == '|'
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
      def parseIntToken(intString: String): Token = {
        try {
          createToken(intString.toInt, intString.length)
        } catch {
          case _: Throwable => createToken(BAD, intString.length)
        }
      }

      def getIntLiteral(chars: List[Char], s: String): (Token, List[Char]) = chars match {
        case (c :: r) if c.isDigit => getIntLiteral(r, s + c)
        case _                     => (parseIntToken(s), chars)
      }
      getIntLiteral(chars, "")
    }

    private def createToken(kind: TokenKind, tokenLength: Int): Token = {
      val token = new Token(kind).setPos(file, Position.encode(line, column))
      column += tokenLength
      token
    }

    private def createToken(integer: Int, tokenLength: Int): Token = {
      val token = new INTLIT(integer).setPos(file, Position.encode(line, column))
      column += tokenLength
      token
    }

    private def createIdToken(string: String, tokenLength: Int): Token = {
      val token = new ID(string).setPos(file, Position.encode(line, column))
      column += tokenLength
      token
    }

    private def createToken(string: String, tokenLength: Int): Token = {
      val token = new STRLIT(string).setPos(file, Position.encode(line, column))
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

    private def readTokens(chars: List[Char]): List[Token] = {
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
        case '+' :: '=' :: r                          => readTokens(r, createToken(PLUSEQ, 2) :: tokens)
        case '-' :: '=' :: r                          => readTokens(r, createToken(MINUSEQ, 2) :: tokens)
        case '*' :: '=' :: r                          => readTokens(r, createToken(MULEQ, 2) :: tokens)
        case '/' :: '=' :: r                          => readTokens(r, createToken(DIVEQ, 2) :: tokens)
        case '%' :: '=' :: r                          => readTokens(r, createToken(MODEQ, 2) :: tokens)
        case '&' :: '=' :: r                          => readTokens(r, createToken(ANDEQ, 2) :: tokens)
        case '|' :: '=' :: r                          => readTokens(r, createToken(OREQ, 2) :: tokens)
        case '^' :: '=' :: r                          => readTokens(r, createToken(XOREQ, 2) :: tokens)
        case '<' :: '<' :: '=' :: r                   => readTokens(r, createToken(LEFTSHIFTEQ, 3) :: tokens)
        case '>' :: '>' :: '=' :: r                   => readTokens(r, createToken(RIGHTSHIFTEQ, 3) :: tokens)
        case '^' :: '=' :: r                          => readTokens(r, createToken(XOREQ, 2) :: tokens)
        case '<' :: '<' :: r                          => readTokens(r, createToken(LEFTSHIFT, 2) :: tokens)
        case '>' :: '>' :: r                          => readTokens(r, createToken(RIGHTSHIFT, 2) :: tokens)
        case '+' :: '+' :: r                          => readTokens(r, createToken(INCREMENT, 2) :: tokens)
        case '-' :: '-' :: r                          => readTokens(r, createToken(DECREMENT, 2) :: tokens)
        case '<' :: '=' :: r                          => readTokens(r, createToken(LESSTHANEQUALS, 2) :: tokens)
        case '>' :: '=' :: r                          => readTokens(r, createToken(GREATERTHANEQUALS, 2) :: tokens)
        case '=' :: '=' :: r                          => readTokens(r, createToken(EQUALS, 2) :: tokens)
        case '!' :: '=' :: r                          => readTokens(r, createToken(NOTEQUALS, 2) :: tokens)
        case '|' :: '|' :: r                          => readTokens(r, createToken(OR, 2) :: tokens)
        case '&' :: '&' :: r                          => readTokens(r, createToken(AND, 2) :: tokens)
        case '0' :: r                                 => readTokens(r, createToken(0, 1) :: tokens)
        case (c :: r) if singleCharTokens.contains(c) => readTokens(r, createToken(singleCharTokens(c), 1) :: tokens)
        case (c :: r) if c.isLetter                   => 
          val (token, tail) = getIdentifierOrKeyword(chars)
          readTokens(tail, token :: tokens)
        case ('"' :: r)                               => 
          val (token, tail) = getStringLiteral(r)
          readTokens(tail, token :: tokens)
        case (c :: r) if c.isDigit                    => 
          val (token, tail) = getIntLiteral(chars)
          readTokens(tail, token :: tokens)
        case Nil                                      => new Token(Tokens.EOF).setPos(file, Position.encode(line, column - 1)) :: tokens
        case _ :: r                                   => readTokens(r, createToken(BAD, 1) :: tokens)
      }
      readTokens(chars, List[Token]()).reverse
    }

    def tokenize(chars: List[Char]): Iterator[Token] = readTokens(chars).iterator
  }

  def run(ctx: Context)(f: File): Iterator[Token] = new Tokenizer(f).tokenize(Source.fromFile(f).buffered.toList)
  def run(chars: List[Char], f: File): Iterator[Token] = new Tokenizer(f).tokenize(chars)
}
