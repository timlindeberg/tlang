package tcompiler
package lexer

import java.io.File
import java.math.BigInteger

import tcompiler.lexer.Tokens._
import tcompiler.utils.{Pipeline, _}

import scala.io.Source

object Lexer extends Pipeline[List[File], List[List[Token]]] {

  def run(ctx: Context)(files: List[File]): List[List[Token]] = {
    files.map { f =>
      new Tokenizer(ctx, f).tokenize(Source.fromFile(f).buffered.toList)
    }
  }
}

class Tokenizer(override var ctx: Context, override val file: File) extends LexerErrors {

  override var line   = 1
  override var column = 1


  def tokenize(chars: List[Char]): List[Token] = {
    def readTokens(chars: List[Char], tokens: List[Token]): List[Token] = chars match {
      case '\n' :: r                              =>
        val token = createToken(NEWLINE, 1)
        column = 1
        line += 1
        // Don't put two newline tokens in a row
        val t = if (tokens.nonEmpty && tokens.head.kind != NEWLINE) token :: tokens else tokens
        readTokens(r, t)
      case (c :: r) if c.isWhitespace             =>
        column += 1
        readTokens(r, tokens)
      case '/' :: '/' :: r                        => readTokens(skipLine(r), tokens)
      case '+' :: '=' :: r                        => readTokens(r, createToken(PLUSEQ, 2) :: tokens)
      case '-' :: '=' :: r                        => readTokens(r, createToken(MINUSEQ, 2) :: tokens)
      case '*' :: '=' :: r                        => readTokens(r, createToken(MULEQ, 2) :: tokens)
      case '/' :: '=' :: r                        => readTokens(r, createToken(DIVEQ, 2) :: tokens)
      case '%' :: '=' :: r                        => readTokens(r, createToken(MODEQ, 2) :: tokens)
      case '&' :: '=' :: r                        => readTokens(r, createToken(ANDEQ, 2) :: tokens)
      case '|' :: '=' :: r                        => readTokens(r, createToken(OREQ, 2) :: tokens)
      case '^' :: '=' :: r                        => readTokens(r, createToken(XOREQ, 2) :: tokens)
      case '<' :: '<' :: '=' :: r                 => readTokens(r, createToken(LEFTSHIFTEQ, 3) :: tokens)
      case '>' :: '>' :: '=' :: r                 => readTokens(r, createToken(RIGHTSHIFTEQ, 3) :: tokens)
      case '<' :: '<' :: r                        => readTokens(r, createToken(LEFTSHIFT, 2) :: tokens)
      case '>' :: '>' :: r                        => readTokens(r, createToken(RIGHTSHIFT, 2) :: tokens)
      case '+' :: '+' :: r                        => readTokens(r, createToken(INCREMENT, 2) :: tokens)
      case '-' :: '-' :: r                        => readTokens(r, createToken(DECREMENT, 2) :: tokens)
      case '<' :: '=' :: r                        => readTokens(r, createToken(LESSTHANEQ, 2) :: tokens)
      case '>' :: '=' :: r                        => readTokens(r, createToken(GREATERTHANEQ, 2) :: tokens)
      case '=' :: '=' :: r                        => readTokens(r, createToken(EQUALS, 2) :: tokens)
      case '!' :: '=' :: r                        => readTokens(r, createToken(NOTEQUALS, 2) :: tokens)
      case '|' :: '|' :: r                        => readTokens(r, createToken(OR, 2) :: tokens)
      case '&' :: '&' :: r                        => readTokens(r, createToken(AND, 2) :: tokens)
      case '?' :: '.' :: r                        => readTokens(r, createToken(SAFEACCESS, 2) :: tokens)
      case '?' :: ':' :: r                        => readTokens(r, createToken(ELVIS, 2) :: tokens)
      case '!' :: '!' :: r                        => readTokens(r, createToken(EXTRACTNULLABLE, 2) :: tokens)
      case '/' :: '*' :: r                        =>
        val (token, tail) = skipBlock(r)
        readTokens(tail, if (token.isDefined) token.get :: tokens else tokens)
      case c :: r if SingleCharTokens.contains(c) => readTokens(r, createToken(SingleCharTokens(c), 1) :: tokens)
      case c :: _ if c.isLetter || c == '_'       =>
        val (token, tail) = getIdentifierOrKeyword(chars)
        readTokens(tail, token :: tokens)
      case '\'' :: r                              =>
        val (token, tail) = getCharLiteral(r)
        readTokens(tail, token :: tokens)
      case '`' :: r                               =>
        val (token, tail) = getMultiLineStringLiteral(r)
        readTokens(tail, token :: tokens)
      case '"' :: r                               =>
        val (token, tail) = getStringLiteral(r)
        readTokens(tail, token :: tokens)
      case '0' :: 'x' :: r                        =>
        val (token, tail) = getHexadecimalLiteral(r)
        readTokens(tail, token :: tokens)
      case '0' :: 'b' :: r                        =>
        val (token, tail) = getBinaryLiteral(r)
        readTokens(tail, token :: tokens)
      case c :: _ if c.isDigit                    =>
        val (token, tail) = getNumberLiteral(chars)
        readTokens(tail, token :: tokens)
      case Nil                                    =>
        line += 1
        column = 1
        createToken(EOF, 1) :: tokens
      case c :: r                                 =>
        ErrorInvalidCharacter(c)
        readTokens(r, createToken(BAD, 1) :: tokens)
    }
    readTokens(chars, List[Token]()).reverse
  }

  private def createIdentifierOrKeyWord(s: String): Token =
    if (Keywords.contains(s)) createToken(Keywords(s), s.length)
    else createIdToken(s, s.length)

  private def getIdentifierOrKeyword(chars: List[Char]): (Token, List[Char]) = {

    def getIdentifierOrKeyword(chars: List[Char], s: String, charsParsed: Int): (Token, List[Char]) = {
      def validChar(c: Char) = c.isLetter || c.isDigit || c == '_'
      chars match {
        case c :: r if validChar(c)    => getIdentifierOrKeyword(r, s + c, charsParsed + 1)
        case c :: _ if isEndingChar(c) => (createIdentifierOrKeyWord(s), chars)
        case c :: r                    =>
          // Advance column here so only the invalid char gets highlighted
          column += charsParsed
          ErrorInvalidIdentifier(c, 1)
          getIdentifierOrKeyword(r, s + c, 1)
        case Nil                       => (createIdentifierOrKeyWord(s), chars)
      }
    }
    getIdentifierOrKeyword(chars.tail, chars.head.toString, 1)
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


  private def getCharLiteral(chars: List[Char]): (Token, List[Char]) = {
    def toEnd(charList: List[Char], length: Int): (Token, List[Char]) =
      charList match {
        case '\'' :: r =>
          ErrorInvalidCharLiteral(length + 1)
          (createToken(BAD, length + 1), r)
        case _ :: r    =>
          toEnd(r, length + 1)
        case Nil       =>
          ErrorUnclosedCharLiteral(length)
          (createToken(BAD, length), Nil)
      }

    chars match {
      case '\'' :: r =>
        ErrorEmptyCharLiteral()
        (createToken(BAD, 2), r)

      // Unicodes
      case '\\' :: 'u' :: r => r match {
        case c1 :: c2 :: c3 :: c4 :: '\'' :: r if areHexDigits(c1, c2, c3, c4) =>
          val unicodeNumber = Integer.parseInt("" + c1 + c2 + c3 + c4, 16)
          (createToken(unicodeNumber.toChar, 8), r)
        case _ :: _ :: _ :: _ :: '\'' :: r                                     =>
          ErrorInvalidUnicode(8)
          (createToken(BAD, 8), r)
        case _ :: _ :: _ :: '\'' :: r                                          =>
          ErrorInvalidUnicode(7)
          (createToken(BAD, 7), r)
        case _ :: _ :: '\'' :: r                                               =>
          ErrorInvalidUnicode(6)
          (createToken(BAD, 6), r)
        case _ :: '\'' :: r                                                    =>
          ErrorInvalidUnicode(5)
          (createToken(BAD, 5), r)
        case '\'' :: r                                                         =>
          ErrorInvalidUnicode(4)
          (createToken(BAD, 4), r)
        case r                                                                 =>
          toEnd(r, 3)
      }

      // Escaped characters
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
          ErrorInvalidEscapeSequence(4)
          (createToken(BAD, 4), r)
      }
      case '\\' :: '\'' :: r      =>
        ErrorInvalidCharLiteral(3)
        (createToken(BAD, 4), r)
      case c :: '\'' :: r         =>
        (createToken(c, 4), r)
      case r                      =>
        toEnd(r, 1)
    }
  }

  private def getStringLiteral(chars: List[Char]): (Token, List[Char]) = {

    val startPos = createToken(BAD, 0)

    def getStringLiteral(chars: List[Char], s: String, charsParsed: Int): (Token, List[Char]) = chars match {
      case '"' :: r  => (createToken(s, s.length + 2), r)
      case '\n' :: _ =>
        ErrorUnclosedStringLiteral(startPos)
        (createToken(BAD, s.length), chars)
      case '\\' :: r => r match {
        case 't' :: r  => getStringLiteral(r, s + '\t', charsParsed + 1)
        case 'b' :: r  => getStringLiteral(r, s + '\b', charsParsed + 1)
        case 'n' :: r  => getStringLiteral(r, s + '\n', charsParsed + 1)
        case 'r' :: r  => getStringLiteral(r, s + '\r', charsParsed + 1)
        case 'f' :: r  => getStringLiteral(r, s + '\f', charsParsed + 1)
        case ''' :: r  => getStringLiteral(r, s + '\'', charsParsed + 1)
        case '"' :: r  => getStringLiteral(r, s + '\"', charsParsed + 1)
        case '\\' :: r => getStringLiteral(r, s + '\\', charsParsed + 1)
        case '[' :: r  => getStringLiteral(r, s + 27.toChar + '[', charsParsed + 1)
        case 'u' :: r  => r match {
          case c1 :: c2 :: c3 :: c4 :: r if areHexDigits(c1, c2, c3, c4) =>
            val unicodeNumber = Integer.parseInt("" + c1 + c2 + c3 + c4, 16)
            getStringLiteral(r, s + unicodeNumber.toChar, +1)
          case _                                                         =>
            // Invalid Unicode
            column += charsParsed
            val firstWhitespace = r.take(5).indexWhere(_.isWhitespace)
            val len = if (firstWhitespace == -1) 5 else firstWhitespace + 1
            ErrorInvalidUnicode(len + 1)
            getStringLiteral(r, s, 2)
        }
        case _         =>
          column += charsParsed
          ErrorInvalidEscapeSequence(2)
          getStringLiteral(r, s, 1)
      }
      case c :: r    => getStringLiteral(r, s + c, charsParsed + 1)
      case Nil       =>
        ErrorUnclosedStringLiteral(startPos)
        (createToken(BAD, s.length), chars)
    }
    getStringLiteral(chars, "", 1)
  }


  private def getHexadecimalLiteral(chars: List[Char]) = {

    def invalid(len: Int) = {
      ErrorInvalidHexadecimalLiteral(len, chars)
      (createToken(BAD, len), chars)
    }

    def getHexadecimalLiteral(chars: List[Char], s: String): (Token, List[Char]) = chars match {
      case '_' :: r                  => getHexadecimalLiteral(r, s)
      case c :: r if isHexDigit(c)   => getHexadecimalLiteral(r, s + c)
      case ('l' | 'L') :: r          => (parseLongToken(s), r)
      case c :: _ if isEndingChar(c) => (parseIntToken(s), chars)
      case _                         => invalid(s.length)
    }

    if (isEndingChar(chars.head))
      invalid(2)
    else
      getHexadecimalLiteral(chars, "0x")
  }

  private def getBinaryLiteral(chars: List[Char]) = {

    def invalid(len: Int) = {
      ErrorInvalidBinaryLiteral(len, chars)
      (createToken(BAD, len), chars)
    }

    def getBinaryLiteral(chars: List[Char], s: String): (Token, List[Char]) = chars match {
      case '_' :: r                  => getBinaryLiteral(r, s)
      case '0' :: r                  => getBinaryLiteral(r, s + '0')
      case '1' :: r                  => getBinaryLiteral(r, s + '1')
      case ('l' | 'L') :: r          => (parseLongToken(s), r)
      case c :: _ if isEndingChar(c) => (parseIntToken(s), chars)
      case _                         => invalid(s.length)
    }

    if (isEndingChar(chars.head))
      invalid(2)
    else
      getBinaryLiteral(chars, "0b")
  }


  private def getNumberLiteral(chars: List[Char]): (Token, List[Char]) = {
    var foundDecimal = false
    var foundE = false
    def getNumberLiteral(chars: List[Char], s: String): (Token, List[Char]) =
      chars match {
        case '_' :: r            => getNumberLiteral(r, s)
        case c :: r if c.isDigit => getNumberLiteral(r, s + c)
        case '.' :: r            =>
          if (!foundDecimal && !foundE) {
            foundDecimal = true
            getNumberLiteral(r, s + ".")
          } else {
            ErrorInvalidNumber(s.length, r)
            (createToken(BAD, s.length), chars)
          }
        case ('e' | 'E') :: r    =>
          if (!foundE) {
            foundE = true
            r match {
              case '-' :: c :: r if c.isDigit => getNumberLiteral(r, s + "E-" + c)
              case c :: r if c.isDigit        => getNumberLiteral(r, s + "E" + c)
              case _                          =>
                ErrorInvalidFloat(s.length, r)
                (createToken(BAD, s.length), chars)
            }
          } else {
            ErrorInvalidFloat(s.length, r)
            (createToken(BAD, s.length), chars)
          }
        case ('f' | 'F') :: r    => (parseFloatToken(s), r)
        case ('l' | 'L') :: r    =>
          if (!foundE && !foundDecimal) {
            (parseLongToken(s), r)
          } else {
            ErrorInvalidFloat(s.length, r)
            (createToken(BAD, s.length), chars)
          }
        case r                   =>
          if (foundDecimal || foundE)
            (parseDoubleToken(s), r)
          else
            (parseIntToken(s), r)
      }

    getNumberLiteral(chars, "")
  }

  private def skipLine(chars: List[Char]): List[Char] = {
    def skip(chars: List[Char]): List[Char] = chars match {
      case '\n' :: _ => chars
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

  private def parseIntToken(numStr: String): Token = {
    def invalid(len: Int) = {
      ErrorNumberTooLargeForInt(len)
      createToken(BAD, len)
    }

    val len = numStr.length
    val isHex = numStr.startsWith("0x")
    val isBin = numStr.startsWith("0b")
    if (isHex || isBin) {
      val num = numStr.drop(2)
      val radix = if (isHex) 16 else 2
      val maxSize = if (isHex) 8 else 32
      if (num.length > maxSize)
        invalid(len)
      else
        createToken(java.lang.Long.parseLong(num, radix).asInstanceOf[Int], len)
    } else {
      try {
        createToken(numStr.toInt, len)
      } catch {
        case _: NumberFormatException => invalid(len)
      }
    }
  }

  private def parseLongToken(numStr: String): Token = {
    def invalid(len: Int) = {
      ErrorNumberTooLargeForLong(len)
      createToken(BAD, len)
    }

    val len = numStr.length
    val isHex = numStr.startsWith("0x")
    val isBin = numStr.startsWith("0b")
    if (isHex || isBin) {
      val num = numStr.drop(2)
      val radix = if (isHex) 16 else 2
      val maxSize = if (isHex) 16 else 64
      if (num.length > maxSize)
        invalid(len)
      else
        createToken(new BigInteger(num, radix).longValue(), len)
    } else {
      try {
        createToken(numStr.toLong, len)
      } catch {
        case _: NumberFormatException => invalid(len)
      }
    }
  }

  // Only accepts valid float and double strings
  private def parseFloatToken(numStr: String): Token = createToken(numStr.toFloat, numStr.length)
  private def parseDoubleToken(numStr: String): Token = createToken(numStr.toDouble, numStr.length)

  private def isEndingChar(c: Char) = c.isWhitespace || SingleCharTokens.contains(c)
  private def areHexDigits(chars: Char*) = chars forall isHexDigit
  private def isHexDigit(c: Char) = c.isDigit || "abcdef".contains(c.toLower)

  private def createToken(int: Int, tokenLength: Int): Token = createToken(new INTLIT(int), tokenLength)
  private def createToken(long: Long, tokenLength: Int): Token = createToken(new LONGLIT(long), tokenLength)
  private def createToken(float: Float, tokenLength: Int): Token = createToken(new FLOATLIT(float), tokenLength)
  private def createToken(double: Double, tokenLength: Int): Token = createToken(new DOUBLELIT(double), tokenLength)
  private def createToken(kind: TokenKind, tokenLength: Int): Token = createToken(new Token(kind), tokenLength)
  private def createIdToken(string: String, tokenLength: Int): Token = createToken(new ID(string), tokenLength)
  private def createToken(char: Char, tokenLength: Int): Token = createToken(new CHARLIT(char), tokenLength)
  private def createToken(string: String, tokenLength: Int): Token = createToken(new STRLIT(string), tokenLength)
  private def createToken[T](token: Token, tokenLength: Int): Token = {
    token.setPos(file, line, column, line, column + tokenLength)
    column += tokenLength
    token
  }
}