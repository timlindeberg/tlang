package koolc
package lexer

import utils._
import scala.io.Source
import java.io.File

object Lexer extends Pipeline[File, Iterator[Token]] {
  import Tokens._

  def run(ctx: Context)(f: File): Iterator[Token] = new Tokenizer(f, ctx).tokenize(Source.fromFile(f).buffered.toList)
  def run(chars: List[Char], f: File): Iterator[Token] = new Tokenizer(f, Context(new Reporter(), None, f)).tokenize(chars)

  class Tokenizer(private val file: File, ctx: Context) {

    def error(msg: String): Unit = error(msg, 0)

    def error(msg: String, colOffset: Int): Unit = {
      val bad = new Token(BAD).setPos(file, Position.encode(line, column + colOffset))
      ctx.reporter.error(msg, bad)
    }

    def error(msg: String, pos: Token): Unit = {
      ctx.reporter.error(msg, pos)
    }

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
      "package"   -> PACKAGE,
      "import"    -> IMPORT,
      "object"    -> OBJECT,
      "class"     -> CLASS,
      "Def"       -> PUBDEF,
      "def"       -> PRIVDEF,
      "protected" -> PROTECTED,
      "var"       -> VAR,
      "main"      -> MAIN,
      "String"    -> STRING,
      "extends"   -> EXTENDS,
      "Unit"      -> UNIT,
      "Int"       -> INT,
      "Long"      -> LONG,
      "Float"     -> FLOAT,
      "Double"    -> DOUBLE,
      "Char"      -> CHAR,
      "Bool"      -> BOOLEAN,
      "while"     -> WHILE,
      "for"       -> FOR,
      "if"        -> IF,
      "else"      -> ELSE,
      "return"    -> RETURN,
      "length"    -> LENGTH,
      "inst"      -> INSTANCEOF,
      "as"        -> AS,
      "true"      -> TRUE,
      "false"     -> FALSE,
      "this"      -> THIS,
      "new"       -> NEW,
      "print"     -> PRINT,
      "println"   -> PRINTLN)

    private var line = 1
    private var column = 1

    private def createIdentifierOrKeyWord(s: String): Token =
      if (keyWords.contains(s)) createToken(keyWords(s), s.length)
      else createIdToken(s, s.length)

    private def getIdentifierOrKeyword(chars: List[Char]): (Token, List[Char]) = {
      def getIdentifierOrKeyword(chars: List[Char], s: String): (Token, List[Char]) = {
        val end = (c: Char) => singleCharTokens.contains(c) || c.isWhitespace
        val validChar = (c: Char) => c.isLetter || c.isDigit || c == '_'
        chars match {
          case (c :: r) if validChar(c) => getIdentifierOrKeyword(r, s + c)
          case (c :: r) if end(c) => (createIdentifierOrKeyWord(s), chars)
          case (c :: r) =>
            error("Invalid identifier.", s.length)
            (createToken(BAD, s.length), chars)
          case Nil => (createIdentifierOrKeyWord(s), chars)
        }
      }
      getIdentifierOrKeyword(chars.tail, chars.head.toString)
    }

    private def getMultiLineStringLiteral(chars: List[Char]): (Token, List[Char]) = {

      val startPos = createToken(BAD, 0)

      def getStringIdentifier(chars: List[Char], s: String): (Token, List[Char]) = chars match {
        case '`' :: r => (createToken(s, s.length + 6), r)
        case '\n' :: r =>
          line += 1
          column = 1
          getStringIdentifier(r, s + '\n')
        case c :: r => getStringIdentifier(r, s + c)
        case Nil =>
          error("Unclosed multiline string literal.", startPos)
          (createToken(BAD, s.length), Nil)
      }
      getStringIdentifier(chars, "")
    }

    private def areHexDigits(chars: Char*) = chars.forall(c => c.isDigit || "abcdef".contains(c.toLower))


    private def getCharLiteral(chars: List[Char]): (Token, List[Char]) = chars match {
        case '\'' :: r =>
          error("Empty character literal.")
          (createToken(BAD, 1), chars)
        case '\\' :: c :: '\'' :: r => c match {
          case 't' => (createToken('\t', 4), r)
          case 'b' => (createToken('\b', 4), r)
          case 'n' => (createToken('\n', 4), r)
          case 'r' => (createToken('\r', 4), r)
          case 'f' => (createToken('\f', 4), r)
          case ''' => (createToken('\'', 4), r)
          case '"' => (createToken('\"', 4), r)
          case '\\' => (createToken('\\', 4), r)
          case _ =>
            error("Invalid escape sequence.", 3)
            (createToken(BAD, 4), r)
        }
        case '\\' :: '\'' :: r =>
          error("Invalid character literal.", 3)
          (createToken(BAD, 4), r)
        case '\\':: 'u' :: r => r match {
          case c1 :: c2 :: c3 :: c4 :: '\'' :: r if areHexDigits(c1, c2, c3, c4) =>
            val unicodeNumber = Integer.parseInt("" + c1 + c2 + c3 + c4, 16)
            (createToken(unicodeNumber.toChar, 8), r)
          case c1 :: c2 :: c3 :: c4 :: '\'' :: r =>
            error("Invalid unicode escape sequence.", 6)
            (createToken(BAD, 4), r)
          case c1 :: c2 :: c3 :: '\'' :: r =>
            error("Invalid unicode escape sequence.", 5)
            (createToken(BAD, 4), r)
          case c1 :: c2 :: '\'' :: r =>
            error("Invalid unicode escape sequence.", 4)
            (createToken(BAD, 4), r)
          case c1 :: '\'' :: r =>
            error("Invalid unicode escape sequence.", 3)
            (createToken(BAD, 4), r)
          case '\'' :: r =>
            error("Invalid unicode escape sequence.", 2)
            (createToken(BAD, 4), r)
        }
        case c :: '\'' :: r => (createToken(c, 4), r)
        case c1 :: c2 :: '\'' :: r =>
          error("Invalid character literal.", 4)
          (createToken(BAD, 4), r)
        case r =>
          error("Unclosed character literal.", 4)
          (createToken(BAD, 4), r)
      }

    private def getStringLiteral(chars: List[Char]): (Token, List[Char]) = {

      val startPos = createToken(BAD, 0)

      def getStringIdentifier(chars: List[Char], s: String): (Token, List[Char]) = chars match {
        case '"' :: r => (createToken(s, s.length + 2), r)
        case '\n' :: r =>
          error("Unclosed string literal.", startPos)
          (createToken(BAD, s.length), chars)
        case '\\' :: r => r match {
          case 't' :: r => getStringIdentifier(r, s + '\t')
          case 'b' :: r => getStringIdentifier(r, s + '\b')
          case 'n' :: r => getStringIdentifier(r, s + '\n')
          case 'r' :: r => getStringIdentifier(r, s + '\r')
          case 'f' :: r => getStringIdentifier(r, s + '\f')
          case ''' :: r => getStringIdentifier(r, s + '\'')
          case '"' :: r => getStringIdentifier(r, s + '\"')
          case '\\' :: r => getStringIdentifier(r, s + '\\')
          case '[' :: r => getStringIdentifier(r, s + 27.toChar + '[') // TODO: add rules for ANSI escape codes
          case 'u' :: r => r match {
            case c1 :: c2 :: c3 :: c4 :: r if areHexDigits(c1, c2, c3, c4) =>
              val unicodeNumber = Integer.parseInt("" + c1 + c2 + c3 + c4, 16)
              getStringIdentifier(r, s + unicodeNumber.toChar)
            case _ =>
              error("Invalid unicode escape sequence.", s.length + 1)
              getStringIdentifier(r, s)
          }
          case _ =>
            error("Invalid escape sequence.", s.length + 1)
            getStringIdentifier(r, s)
        }
        case c :: r => getStringIdentifier(r, s + c)
        case Nil =>
          error("Unclosed string literal.", startPos)
          (createToken(BAD, s.length), chars)
      }
      getStringIdentifier(chars, "")
    }

    private def getNumberLiteral(chars: List[Char]): (Token, List[Char]) = {


      val startPos = createToken(BAD, 0)

      def tryConversion[T](numStr: String, conversion: (String) => T): Option[T] =
        try {
          Some(conversion(numStr))
        } catch {
          case _: NumberFormatException =>
            error("Number is too large to fit in datatype." , startPos)
            None
        }

      def parseIntToken(numStr: String): Token = tryConversion(numStr, _.toInt) match {
        case Some(value) => createToken(value, numStr.length)
        case None        => createToken(BAD, numStr.length)
      }

      def parseLongToken(numStr: String): Token = tryConversion(numStr, _.toLong) match {
        case Some(value) => createToken(value, numStr.length)
        case None        => createToken(BAD, numStr.length)
      }

      def parseFloatToken(numStr: String): Token = tryConversion(numStr, _.toFloat) match {
        case Some(value) => createToken(value, numStr.length)
        case None        => createToken(BAD, numStr.length)
      }

      def parseDoubleToken(numStr: String): Token = tryConversion(numStr, _.toDouble) match {
        case Some(value) => createToken(value, numStr.length)
        case None        => createToken(BAD, numStr.length)
      }

      var foundDecimal = false
      def getNumberLiteral(chars: List[Char], s: String): (Token, List[Char]) =
        chars match {
          case c   :: r if c.isDigit => getNumberLiteral(r, s + c)
          case '.' :: r =>
            if (!foundDecimal) {
              foundDecimal = true
              getNumberLiteral(r, s + ".")
            } else {
              error("Invalid number.", startPos)
              (createToken(BAD, s.length), chars)
            }
          case 'f' :: r => (parseFloatToken(s), r)
          case 'l' :: r => (parseLongToken(s), r)
          case _        =>
            if(foundDecimal) (parseDoubleToken(s), chars)
            else             (parseIntToken(s), chars)
        }

      val res = getNumberLiteral(chars, "")
      res._1.setPos(startPos)
      res
    }


    private def createToken(int: Int, tokenLength: Int)         = _createToken(new INTLIT(int), tokenLength)
    private def createToken(long: Long, tokenLength: Int)       = _createToken(new LONGLIT(long), tokenLength)
    private def createToken(float: Float, tokenLength: Int)     = _createToken(new FLOATLIT(float), tokenLength)
    private def createToken(double: Double, tokenLength: Int)   = _createToken(new DOUBLELIT(double), tokenLength)
    private def createToken(kind: TokenKind, tokenLength: Int)  = _createToken(new Token(kind), tokenLength)
    private def createIdToken(string: String, tokenLength: Int) = _createToken(new ID(string), tokenLength)
    private def createToken(char: Char, tokenLength: Int)       = _createToken(new CHARLIT(char), tokenLength)
    private def createToken(string: String, tokenLength: Int)   = _createToken(new STRLIT(string), tokenLength)

    private def _createToken[T](token:  Token, tokenLength: Int): Token = {
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
        case '/' :: '/' :: r                          => readTokens(skipLine(r), tokens)
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
        case '/' :: '*' :: r =>
          val (token, tail) = skipBlock(r)
          readTokens(tail, if (token.isDefined) token.get :: tokens else tokens)
        case c :: r if singleCharTokens.contains(c) => readTokens(r, createToken(singleCharTokens(c), 1) :: tokens)
        case c :: r if c.isLetter =>
          val (token, tail) = getIdentifierOrKeyword(chars)
          readTokens(tail, token :: tokens)
        case '\'' :: r =>
          val (token, tail) = getCharLiteral(r)
          readTokens(tail, token :: tokens)
        case '`' ::  r =>
          val (token, tail) = getMultiLineStringLiteral(r)
          readTokens(tail, token :: tokens)
        case '"' :: r =>
          val (token, tail) = getStringLiteral(r)
          readTokens(tail, token :: tokens)
        case c :: r if c.isDigit =>
          val (token, tail) = getNumberLiteral(chars)
          readTokens(tail, token :: tokens)
        case Nil                                      => new Token(Tokens.EOF).setPos(file, Position.encode(line, column - 1)) :: tokens
        case _ :: r                                   =>
          error("Invalid character.")
          readTokens(r, createToken(BAD, 1) :: tokens)
      }
      readTokens(chars, List[Token]()).reverse
    }

    def tokenize(chars: List[Char]): Iterator[Token] = readTokens(chars).iterator
  }


}
