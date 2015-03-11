package koolc
package lexer

import utils._
import scala.io.Source
import java.io.File
import scala.collection.mutable.ArrayBuffer
import scala.runtime.RichChar

object Lexer extends Pipeline[File, Iterator[Token]] {
  import Tokens._

  object Tokenizer {

    val singleCharTokens = Map(
      ':' -> Tokens.COLON,
      ';' -> Tokens.SEMICOLON,
      '.' -> Tokens.DOT,
      ',' -> Tokens.COMMA,
      '!' -> Tokens.BANG,
      '(' -> Tokens.LPAREN,
      ')' -> Tokens.RPAREN,
      '[' -> Tokens.LBRACKET,
      ']' -> Tokens.RBRACKET,
      '{' -> Tokens.LBRACE,
      '}' -> Tokens.RBRACE,
      '+' -> Tokens.PLUS,
      '-' -> Tokens.MINUS,
      '*' -> Tokens.TIMES,
      '<' -> Tokens.LESSTHAN,
      '/' -> Tokens.DIV)

    val keyWords = Map(
      "object" -> Tokens.OBJECT,
      "class" -> Tokens.CLASS,
      "def" -> Tokens.DEF,
      "var" -> Tokens.VAR,
      "Unit" -> Tokens.UNIT,
      "main" -> Tokens.MAIN,
      "String" -> Tokens.STRING,
      "extends" -> Tokens.EXTENDS,
      "Int" -> Tokens.INT,
      "Bool" -> Tokens.BOOLEAN,
      "while" -> Tokens.WHILE,
      "if" -> Tokens.IF,
      "else" -> Tokens.ELSE,
      "return" -> Tokens.RETURN,
      "length" -> Tokens.LENGTH,
      "true" -> Tokens.TRUE,
      "false" -> Tokens.FALSE,
      "this" -> Tokens.THIS,
      "new" -> Tokens.NEW,
      "println" -> Tokens.PRINTLN)

    def tokenize(file: File): Iterator[Token] = {
      val buffer = Source.fromFile(file).buffered.toList
      var tokens = new ArrayBuffer[Token]()

      var s = new StringBuilder
      var line = 1
      var column = 1

      var index = 0

      val next = () => {
        column += 1
        var c = buffer(index)
        index += 1
        if (c == '\n') {
          line += 1
          column = 1
        }
        c
      }

      val peek = () => {
        buffer(index + 1)
      }

      val addToken = (token: Token, line: Int, column: Int) => {
        token.setPos(file, Position.encode(line, column))
        tokens += token
      }

      while (index < buffer.size) {
        var c = next()

        if (singleCharTokens.contains(c)) {
          addToken(new Token(singleCharTokens(c)), line, column - 1)
        } else if (!c.isWhitespace) {

          var specialPeople = () => {
            c match {
              case '/' =>
                {
                  c = next()
                  c match {
                    case '/' => {
                      while (next() != '\n') {}
                    }
                    case '*' => {
                      while (next() != '*') {}
                      c = next()
                      if (c != '/') {
                        addToken(new Token(Tokens.BAD), line, column - 1)
                      }
                    }
                    case default => {
                      addToken(new Token(Tokens.BAD), line, column - 1)
                    }
                  }
                  true
                }
              case '=' => {
                c = peek()
                if (c == '=') {
                  c = next()
                  addToken(new Token(Tokens.EQUALS), line, column - 2)
                } else {
                  addToken(new Token(Tokens.EQSIGN), line, column - 1)
                }
                true
              }
              case '&' => {
                c = peek()
                if (c == '&') {
                  c = next()
                  addToken(new Token(Tokens.AND), line, column - 2)
                } else {
                  addToken(new Token(Tokens.BAD), line, column - 1)
                }
                true
              }
              case '|' => {
                c = peek()
                if (c == '|') {
                  c = next()
                  addToken(new Token(Tokens.OR), line, column - 2)
                } else {
                  addToken(new Token(Tokens.BAD), line, column - 1)
                }
                true
              }
              case '"' => {
                var stringb = new StringBuilder
                c = next()
                while (c != '"') { // TODO
                  stringb += c
                  c = next()
                }
                var str = stringb.toString
                addToken(new Tokens.STRLIT(str), line, column - str.length - 1)
                true
              }
              case isDigit => ???
              case default => false
            }
          }

          if (!specialPeople()) {
            var doIt = () => {
              c = next()
              !singleCharTokens.contains(c) && c != ' ' && c != '\n' && c != '\t'
            }

            var s = new StringBuilder
            s += c

            while (doIt()) {
              s += c
            }

            var token = s.mkString
            if (keyWords.contains(token)) {
              addToken(new Token(keyWords(token)), line, column - token.length - 1)
              if (singleCharTokens.contains(c)) {
                addToken(new Token(singleCharTokens(c)), line, column - 1)
              }
            } else {
              addToken(new Tokens.ID(token), line, column - token.length - 1)
            }
          }

        }
      }
      addToken(new Token(Tokens.EOF), line, column - 1)

      tokens.iterator
    }
  }

  def run(ctx: Context)(f: File): Iterator[Token] = {
    import ctx.reporter._

    // Complete this file
    Tokenizer.tokenize(f)
  }
}
