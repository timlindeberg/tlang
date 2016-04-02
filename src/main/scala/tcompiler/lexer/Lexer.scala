package tcompiler
package lexer

import java.io.File
import tcompiler.utils._
import Tokens._
import scala.io.Source

object Lexer extends Pipeline[File, List[Token]] {

  def run(ctx: Context)(f: File): List[Token] = new Tokenizer(f, ctx).tokenize(Source.fromFile(f).buffered.toList)
  def run(chars: List[Char], f: File): List[Token] = new Tokenizer(f, Context(new Reporter(), None, f)).tokenize(chars)

}


class Tokenizer(val file: File, ctx: Context) {

  private var line = 1
  private var column = 1

  def tokenize(chars: List[Char]): List[Token] = {
    def readTokens(chars: List[Char], tokens: List[Token]): List[Token] = chars match {
      case '\n' :: r                                        =>
        val token = createToken(NEWLINE, 1)
        column = 1
        line += 1
        // Don't put two newline tokens in a row
        val t = if(tokens.nonEmpty && tokens.head.kind != NEWLINE) token :: tokens else tokens
        readTokens(r, t)
      case (c :: r) if c.isWhitespace                       =>
        column += 1
        readTokens(r, tokens)
      case '/' :: '/' :: r                                  => readTokens(skipLine(r), tokens)
      case '+' :: '=' :: r                                  => readTokens(r, createToken(PLUSEQ, 2) :: tokens)
      case '-' :: '=' :: r                                  => readTokens(r, createToken(MINUSEQ, 2) :: tokens)
      case '*' :: '=' :: r                                  => readTokens(r, createToken(MULEQ, 2) :: tokens)
      case '/' :: '=' :: r                                  => readTokens(r, createToken(DIVEQ, 2) :: tokens)
      case '%' :: '=' :: r                                  => readTokens(r, createToken(MODEQ, 2) :: tokens)
      case '&' :: '=' :: r                                  => readTokens(r, createToken(ANDEQ, 2) :: tokens)
      case '|' :: '=' :: r                                  => readTokens(r, createToken(OREQ, 2) :: tokens)
      case '^' :: '=' :: r                                  => readTokens(r, createToken(XOREQ, 2) :: tokens)
      case '<' :: '<' :: '=' :: r                           => readTokens(r, createToken(LEFTSHIFTEQ, 3) :: tokens)
      case '>' :: '>' :: '=' :: r                           => readTokens(r, createToken(RIGHTSHIFTEQ, 3) :: tokens)
      case '<' :: '<' :: r                                  => readTokens(r, createToken(LEFTSHIFT, 2) :: tokens)
      case '>' :: '>' :: r                                  => readTokens(r, createToken(RIGHTSHIFT, 2) :: tokens)
      case '+' :: '+' :: r                                  => readTokens(r, createToken(INCREMENT, 2) :: tokens)
      case '-' :: '-' :: r                                  => readTokens(r, createToken(DECREMENT, 2) :: tokens)
      case '<' :: '=' :: r                                  => readTokens(r, createToken(LESSTHANEQUALS, 2) :: tokens)
      case '>' :: '=' :: r                                  => readTokens(r, createToken(GREATERTHANEQUALS, 2) :: tokens)
      case '=' :: '=' :: r                                  => readTokens(r, createToken(EQUALS, 2) :: tokens)
      case '!' :: '=' :: r                                  => readTokens(r, createToken(NOTEQUALS, 2) :: tokens)
      case '|' :: '|' :: r                                  => readTokens(r, createToken(OR, 2) :: tokens)
      case '&' :: '&' :: r                                  => readTokens(r, createToken(AND, 2) :: tokens)
      case '/' :: '*' :: r                                  =>
        val (token, tail) = skipBlock(r)
        readTokens(tail, if (token.isDefined) token.get :: tokens else tokens)
      case c :: r if Tokenizer.SingleCharTokens.contains(c) => readTokens(r, createToken(Tokenizer.SingleCharTokens(c), 1) :: tokens)
      case c :: r if c.isLetter                             =>
        val (token, tail) = getIdentifierOrKeyword(chars)
        readTokens(tail, token :: tokens)
      case '\'' :: r                                        =>
        val (token, tail) = getCharLiteral(r)
        readTokens(tail, token :: tokens)
      case '`' :: r                                         =>
        val (token, tail) = getMultiLineStringLiteral(r)
        readTokens(tail, token :: tokens)
      case '"' :: r                                         =>
        val (token, tail) = getStringLiteral(r)
        readTokens(tail, token :: tokens)
      case c :: r if c.isDigit                              =>
        val (token, tail) = getNumberLiteral(chars)
        readTokens(tail, token :: tokens)
      case Nil                                              => createToken(EOF, 1) :: tokens
      case c :: r                                           =>
        ErrorInvalidCharacter(c)
        readTokens(r, createToken(BAD, 1) :: tokens)
    }
    readTokens(chars, List[Token]()).reverse
  }


  private def createIdentifierOrKeyWord(s: String): Token =
    if (Tokenizer.KeyWords.contains(s)) createToken(Tokenizer.KeyWords(s), s.length)
    else createIdToken(s, s.length)

  private def getIdentifierOrKeyword(chars: List[Char]): (Token, List[Char]) = {
    def getIdentifierOrKeyword(chars: List[Char], s: String): (Token, List[Char]) = {
      val end = (c: Char) => Tokenizer.SingleCharTokens.contains(c) || c.isWhitespace
      val validChar = (c: Char) => c.isLetter || c.isDigit || c == '_'
      chars match {
        case c :: r if validChar(c) => getIdentifierOrKeyword(r, s + c)
        case c :: r if end(c)       => (createIdentifierOrKeyWord(s), chars)
        case c :: r                 =>
          ErrorInvalidIdentifier(c, s.length)
          (createToken(BAD, s.length), chars)
        case Nil                      => (createIdentifierOrKeyWord(s), chars)
      }
    }
    getIdentifierOrKeyword(chars.tail, chars.head.toString)
  }

  private def getMultiLineStringLiteral(chars: List[Char]): (Token, List[Char]) = {
    val startPos = createToken(BAD, 0)

    def getStringIdentifier(chars: List[Char], s: String): (Token, List[Char]) = chars match {
      case '`' :: r  => (createToken(s, s.length + 2), r)
      case '\n' :: r =>
        line += 1
        column = 1
        getStringIdentifier(r, s + '\n')
      case c :: r    => getStringIdentifier(r, s + c)
      case Nil       =>
        ErrorUnclosedMultilineString(startPos)
        (createToken(BAD, s.length), Nil)
    }
    getStringIdentifier(chars, "")
  }


  private def getCharLiteral(chars: List[Char]): (Token, List[Char]) = chars match {
    case '\'' :: '\'' :: r      => (createToken('\'', 3), r)
    case '\'' :: r              =>
      ErrorEmptyCharLiteral()
      (createToken(BAD, 1), chars)
    case '\\' :: c :: '\'' :: r => c match {
      case 't'  => (createToken('\t', 4), r)
      case 'b'  => (createToken('\b', 4), r)
      case 'n'  => (createToken('\n', 4), r)
      case 'r'  => (createToken('\r', 4), r)
      case 'f'  => (createToken('\f', 4), r)
      case '''  => (createToken('\'', 4), r)
      case '"'  => (createToken('\"', 4), r)
      case '\\' => (createToken('\\', 4), r)
      case _    =>
        ErrorInvalidEscapeSequence(3)
        (createToken(BAD, 4), r)
    }
    case '\\' :: '\'' :: r      =>
      ErrorInvalidCharLiteral(3)
      (createToken(BAD, 4), r)
    case '\\' :: 'u' :: r       => r match {
      case c1 :: c2 :: c3 :: c4 :: '\'' :: r if areHexDigits(c1, c2, c3, c4) =>
        val unicodeNumber = Integer.parseInt("" + c1 + c2 + c3 + c4, 16)
        (createToken(unicodeNumber.toChar, 8), r)
      case c1 :: c2 :: c3 :: c4 :: '\'' :: r                                 =>
        ErrorInvalidUnicode(6)
        (createToken(BAD, 4), r)
      case c1 :: c2 :: c3 :: '\'' :: r                                       =>
        ErrorInvalidUnicode(5)
        (createToken(BAD, 4), r)
      case c1 :: c2 :: '\'' :: r                                             =>
        ErrorInvalidUnicode(4)
        (createToken(BAD, 4), r)
      case c1 :: '\'' :: r                                                   =>
        ErrorInvalidUnicode(3)
        (createToken(BAD, 4), r)
      case '\'' :: r                                                         =>
        ErrorInvalidUnicode(2)
        (createToken(BAD, 4), r)
    }
    case c :: '\'' :: r         => (createToken(c, 4), r)
    case c1 :: c2 :: '\'' :: r  =>
      ErrorInvalidCharLiteral(4)
      (createToken(BAD, 4), r)
    case r                      =>
      ErrorUnclosedCharLiteral(4)
      (createToken(BAD, 4), r)
  }

  private def getStringLiteral(chars: List[Char]): (Token, List[Char]) = {

    val startPos = createToken(BAD, 0)

    def getStringIdentifier(chars: List[Char], s: String): (Token, List[Char]) = chars match {
      case '"' :: r  => (createToken(s, s.length + 2), r)
      case '\n' :: r =>
        ErrorUnclosedStringLiteral(startPos)
        (createToken(BAD, s.length), chars)
      case '\\' :: r => r match {
        case 't' :: r  => getStringIdentifier(r, s + '\t')
        case 'b' :: r  => getStringIdentifier(r, s + '\b')
        case 'n' :: r  => getStringIdentifier(r, s + '\n')
        case 'r' :: r  => getStringIdentifier(r, s + '\r')
        case 'f' :: r  => getStringIdentifier(r, s + '\f')
        case ''' :: r  => getStringIdentifier(r, s + '\'')
        case '"' :: r  => getStringIdentifier(r, s + '\"')
        case '\\' :: r => getStringIdentifier(r, s + '\\')
        case '[' :: r  => getStringIdentifier(r, s + 27.toChar + '[')
        case 'u' :: r  => r match {
          case c1 :: c2 :: c3 :: c4 :: r if areHexDigits(c1, c2, c3, c4) =>
            val unicodeNumber = Integer.parseInt("" + c1 + c2 + c3 + c4, 16)
            getStringIdentifier(r, s + unicodeNumber.toChar)
          case _                                                         =>
            ErrorInvalidUnicode(s.length + 1)
            getStringIdentifier(r, s)
        }
        case _         =>
          ErrorInvalidEscapeSequence(s.length + 1)
          getStringIdentifier(r, s)
      }
      case c :: r    => getStringIdentifier(r, s + c)
      case Nil       =>
        ErrorUnclosedStringLiteral(startPos)
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
          ErrorNumberTooLarge(startPos)
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
    var foundE = false
    def getNumberLiteral(chars: List[Char], s: String): (Token, List[Char]) =
      chars match {
        case c :: r if c.isDigit => getNumberLiteral(r, s + c)
        case '.' :: r            =>
          if (!foundDecimal && !foundE) {
            foundDecimal = true
            getNumberLiteral(r, s + ".")
          } else {
            ErrorInvalidNumber(startPos)
            (createToken(BAD, s.length), chars)
          }
        case ('e' | 'E') :: r    =>
          if (!foundE && foundDecimal) {
            foundE = true
            r match {
              case '-' :: c :: r if c.isDigit => getNumberLiteral(r, s + "E-" + c)
              case c :: r if c.isDigit        => getNumberLiteral(r, s + "E" + c)
              case _                          =>
                ErrorInvalidFloat(startPos)
                (createToken(BAD, s.length), chars)
            }

          } else {
            ErrorInvalidFloat(startPos)
            (createToken(BAD, s.length), chars)
          }
        case ('f' | 'F') :: r    => (parseFloatToken(s), r)
        case ('l' | 'L') :: r    =>
          if (!foundE && !foundDecimal) {
            (parseLongToken(s), r)
          } else {
            ErrorInvalidFloat(startPos)
            (createToken(BAD, s.length), chars)
          }
        case _                   =>
          if (foundDecimal) (parseDoubleToken(s), chars)
          else (parseIntToken(s), chars)
      }

    val res = getNumberLiteral(chars, "")
    res._1.setPos(startPos)
    res
  }

  private def skipLine(chars: List[Char]): List[Char] = {
    def skip(chars: List[Char]): List[Char] = chars match {
      case '\n' :: r => chars
      case _ :: r    =>
        column += 1
        skip(r)
      case Nil       => Nil
    }
    column += 2
    skip(chars)
  }

  private def skipBlock(chars: List[Char]): (Option[Token], List[Char]) = {
    def skip(chars: List[Char]): (Option[Token], List[Char]) = chars match {
      case '*' :: '/' :: r =>
        column += 2
        (None, r)
      case '\n' :: r       =>
        line += 1
        column = 1
        skip(r)
      case _ :: r          =>
        column += 1
        skip(r)
      case Nil             => (Some(createToken(BAD, 1)), Nil)
    }
    column += 2
    skip(chars)
  }

  private def areHexDigits(chars: Char*) = chars.forall(c => c.isDigit || "abcdef".contains(c.toLower))

  private def createToken(int: Int, tokenLength: Int): Token = createToken(new INTLIT(int), tokenLength)
  private def createToken(long: Long, tokenLength: Int): Token = createToken(new LONGLIT(long), tokenLength)
  private def createToken(float: Float, tokenLength: Int): Token = createToken(new FLOATLIT(float), tokenLength)
  private def createToken(double: Double, tokenLength: Int): Token = createToken(new DOUBLELIT(double), tokenLength)
  private def createToken(kind: TokenKind, tokenLength: Int): Token = createToken(new Token(kind), tokenLength)
  private def createIdToken(string: String, tokenLength: Int): Token = createToken(new ID(string), tokenLength)
  private def createToken(char: Char, tokenLength: Int): Token = createToken(new CHARLIT(char), tokenLength)
  private def createToken(string: String, tokenLength: Int): Token = createToken(new STRLIT(string), tokenLength)
  private def createToken[T](token: Token, tokenLength: Int): Token = {
    token.setPos(file, Position.encode(line, column))
    column += tokenLength
    token
  }

  private def error(errorCode: Int, msg: String, colOffset: Int): Unit = {
    val bad = new Token(BAD).setPos(file, Position.encode(line, column + colOffset))
    error(errorCode, msg, bad)
  }

  private def error(errorCode: Int, msg: String, pos: Token): Unit =
    ctx.reporter.error("L", errorCode, msg, pos)


  //---------------------------------------------------------------------------------------
  //  Error messages
  //---------------------------------------------------------------------------------------

  private def ErrorInvalidCharacter(c: Char) =
    error(0, s"Invalid character: '$c'.", 0)

  private def ErrorInvalidIdentifier(c: Char, length: Int) =
    error(1, s"Invalid character in identifier: '$c'.", length)

  private def ErrorUnclosedMultilineString(pos: Token) =
    error(2, "Unclosed multiline string literal.", pos)

  private def ErrorEmptyCharLiteral() =
    error(3, "Empty character literal.", 0)

  private def ErrorInvalidEscapeSequence(length: Int) =
    error(4, "Invalid escape sequence.", length)

  private def ErrorInvalidCharLiteral(length: Int) =
    error(5, "Invalid character literal.", length)

  private def ErrorInvalidUnicode(length: Int) =
    error(6, "Invalid unicode escape sequence.", length)

  private def ErrorUnclosedCharLiteral(length: Int) =
    error(7, "Unclosed character literal.", length)

  private def ErrorUnclosedStringLiteral(pos: Token) =
    error(8, "Unclosed string literal.", pos)

  private def ErrorNumberTooLarge(pos: Token) =
    error(9, "Number is too large to fit in datatype.", pos)

  private def ErrorInvalidNumber(pos: Token) =
    error(10, "Invalid number.", pos)

  private def ErrorInvalidFloat(pos: Token) =
    error(11, "Invalid floating point number.", pos)


}

object Tokenizer {

  private val SingleCharTokens = Map(
    ':' -> COLON,
    ';' -> SEMICOLON,
    '.' -> DOT,
    ',' -> COMMA,
    '!' -> BANG,
    '#' -> HASH,
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
    '≠' -> NOTEQUALS,
    '?' -> QUESTIONMARK,
    '%' -> MODULO,
    '~' -> LOGICNOT,
    '&' -> LOGICAND,
    '|' -> LOGICOR,
    '^' -> LOGICXOR
  )

  private val KeyWords = Map(
    "package" -> PACKAGE,
    "import" -> IMPORT,
    "object" -> OBJECT,
    "class" -> CLASS,
    "Def" -> PUBDEF,
    "def" -> PRIVDEF,
    "protected" -> PROTECTED,
    "Var" -> PUBVAR,
    "var" -> PRIVVAR,
    "static" -> STATIC,
    "String" -> STRING,
    "extends" -> EXTENDS,
    "Unit" -> UNIT,
    "Int" -> INT,
    "Long" -> LONG,
    "Float" -> FLOAT,
    "Double" -> DOUBLE,
    "Char" -> CHAR,
    "Bool" -> BOOLEAN,
    "while" -> WHILE,
    "for" -> FOR,
    "if" -> IF,
    "else" -> ELSE,
    "return" -> RETURN,
    "length" -> LENGTH,
    "is" -> INSTANCEOF,
    "as" -> AS,
    "true" -> TRUE,
    "false" -> FALSE,
    "this" -> THIS,
    "new" -> NEW,
    "implicit" -> IMPLICIT,
    "print" -> PRINT,
    "println" -> PRINTLN,
    "error" -> ERROR
  )

}