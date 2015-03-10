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

    val singleTokens = Map(
      ':' -> { () => new Token(Tokens.COLON) },
      ';' -> { () => new Token(Tokens.SEMICOLON) },
      '.' -> { () => new Token(Tokens.DOT) },
      ',' -> { () => new Token(Tokens.COMMA) },
      '!' -> { () => new Token(Tokens.BANG) },
      '(' -> { () => new Token(Tokens.LPAREN) },
      ')' -> { () => new Token(Tokens.RPAREN) },
      '[' -> { () => new Token(Tokens.LBRACKET) },
      ']' -> { () => new Token(Tokens.RBRACKET) },
      '{' -> { () => new Token(Tokens.LBRACE) },
      '}' -> { () => new Token(Tokens.RBRACE) },
      '+' -> { () => new Token(Tokens.PLUS) },
      '-' -> { () => new Token(Tokens.MINUS) },
      '*' -> { () => new Token(Tokens.TIMES) },
      '<' -> { () => new Token(Tokens.LESSTHAN) },
      '/' -> { () => new Token(Tokens.DIV) })

    val keyWords = Map(
      "object" -> { () => new Token(Tokens.OBJECT) },
      "class" -> { () => new Token(Tokens.CLASS) },
      "def" -> { () => new Token(Tokens.DEF) },
      "var" -> { () => new Token(Tokens.VAR) },
      "Unit" -> { () => new Token(Tokens.UNIT) },
      "main" -> { () => new Token(Tokens.MAIN) },
      "String" -> { () => new Token(Tokens.STRING) },
      "extends" -> { () => new Token(Tokens.EXTENDS) },
      "Int" -> { () => new Token(Tokens.INT) },
      "Bool" -> { () => new Token(Tokens.BOOLEAN) },
      "while" -> { () => new Token(Tokens.WHILE) },
      "if" -> { () => new Token(Tokens.IF) },
      "else" -> { () => new Token(Tokens.ELSE) },
      "return" -> { () => new Token(Tokens.RETURN) },
      "length" -> { () => new Token(Tokens.LENGTH) },
      "true" -> { () => new Token(Tokens.TRUE) },
      "false" -> { () => new Token(Tokens.FALSE) },
      "this" -> { () => new Token(Tokens.THIS) },
      "new" -> { () => new Token(Tokens.NEW) },
      "println" -> { () => new Token(Tokens.PRINTLN) })

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
        buffer(index+1)
      }

      val addToken = (token: Token, line: Int, column: Int) => {
        token.setPos(file, Position.encode(line, column))
        tokens += token
      }

      while (index < buffer.size) {
        var c = next()

        if (singleTokens.contains(c)) {
          addToken(singleTokens(c)(), line, column - 1)
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
                if(c == '&'){
                   c = next()
                   addToken(new Token(Tokens.AND), line, column - 2)
                }else{
                  addToken(new Token(Tokens.BAD), line, column - 1)
                }
                true
              }
              case '|' => {
                c = peek()
                if(c == '|'){
                   c = next()
                   addToken(new Token(Tokens.OR), line, column - 2)
                }else{
                  addToken(new Token(Tokens.BAD), line, column - 1)
                }
                true
              }
              case '"' => {
                var stringb = new StringBuilder
                c = next()
                while(c != '"'){ // TODO
                  stringb += c
                  c = next()
                }
                var str = stringb.toString
                addToken(new Tokens.STRLIT(str), line, column - str.length - 1)
                true
              }
              case default => false
              //              case '&'     => ??? // &&
              //              case '|'     => ??? // ||
              //              case '"'     => ???
              //              case isDigit => ???
            }
          }

          if (!specialPeople()) {
            var doIt = () => {
              c = next()
              !singleTokens.contains(c) && c != ' ' && c != '\n' && c != '\t'
            }

            var s = new StringBuilder
            s += c

            while (doIt()) {
              s += c
            }

            var token = s.mkString
            if (keyWords.contains(token)) {
              addToken(keyWords(token)(), line, column - token.length - 1)
              if (singleTokens.contains(c)) {
                addToken(singleTokens(c)(), line, column - 1)
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
