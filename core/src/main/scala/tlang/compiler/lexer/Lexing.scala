package tlang.compiler
package lexer

import java.math.BigInteger

import tlang.Context
import tlang.compiler.lexer.Tokens._
import tlang.utils.Extensions._
import tlang.utils.Source
import tlang.utils.formatting.Formatting

import scala.annotation.tailrec

object Lexing extends CompilerPhase[Source, List[Token]] {

  override protected def run(ctx: Context)(inputs: List[Source]): List[List[Token]] = {
    inputs.map { source =>
      val lexer = new Lexer(ctx, source)
      lexer()
    }
  }

  override def description(formatting: Formatting): String =
    """
      |Lexes the input and produces tokens.
    """.stripMargin.trim

  override def printDebugOutput(output: List[List[Token]], formatting: Formatting): Unit = {
    print(DebugOutputFormatter(name, formatting).formatTokens(output))
  }
}

class Lexer(override val ctx: Context, override val source: Source) extends LexerErrors {

  protected override var line   = 1
  protected override var column = 1
  protected          var indent = 0

  def apply(): List[Token] = {

    @tailrec def readTokens(chars: List[Char], tokens: List[Token]): List[Token] = chars match {
      case '\n' :: _                  =>
        val (newlinesParsed, r) = parseCharacters(chars, '\n')
        val newlineToken = new Token(NEWLINE).setPos(source, line, column, line + newlinesParsed, 1)
        column = 1
        line += newlinesParsed
        val (indentToken, rest) = getIndentToken(r)
        readTokens(rest, (indentToken :+ newlineToken) ::: tokens)
      case '\t' :: _                  =>
        val (tabsParsed, r) = parseCharacters(chars, '\t')
        report(TabsNonIndentation(tabsParsed))
        column += tabsParsed
        readTokens(r, tokens)
      case (c :: r) if c.isWhitespace =>
        column += 1
        readTokens(r, tokens)
      case '/' :: '/' :: r            =>
        val (token, tail) = lineComment(r)
        readTokens(tail, token :: tokens)
      case '/' :: '*' :: r            =>
        val (token, tail) = blockComment(r)
        readTokens(tail, token :: tokens)
      // Prioritise longer tokens
      case c1 :: c2 :: c3 :: r if tokenExists(s"$c1$c2$c3") => readTokens(r, createToken(NonKeywords(s"$c1$c2$c3"), 3) :: tokens)
      case c1 :: c2 :: r if tokenExists(s"$c1$c2")          => readTokens(r, createToken(NonKeywords(s"$c1$c2"), 2) :: tokens)
      case c :: r if tokenExists(s"$c")                     => readTokens(r, createToken(NonKeywords(s"$c"), 1) :: tokens)

      case c :: _ if c.isLetter || c == '_' =>
        val (token, tail) = getIdentifierOrKeyword(chars)
        readTokens(tail, token :: tokens)
      case '\'' :: r                        =>
        val (token, tail) = getCharLiteral(r)
        readTokens(tail, token :: tokens)
      case '"' :: r                         =>
        val (token, tail) = getStringLiteral(r)
        readTokens(tail, token :: tokens)
      case '`' :: r                         =>
        val (token, tail) = getMultiLineStringLiteral(r)
        readTokens(tail, token :: tokens)
      case '0' :: 'x' :: r                  =>
        val (token, tail) = getHexadecimalLiteral(r)
        readTokens(tail, token :: tokens)
      case '0' :: 'b' :: r                  =>
        val (token, tail) = getBinaryLiteral(r)
        readTokens(tail, token :: tokens)
      case c :: _ if c.isDigit              =>
        val (token, tail) = getNumberLiteral(chars)
        readTokens(tail, token :: tokens)
      case Nil                              =>
        val dedent = (0 until indent).map(_ => createToken(DEDENT, 0)).toList
        val eof = if (tokens.nonEmpty && tokens.head.kind != DEDENT && tokens.head.kind != NEWLINE)
          createToken(EOF, 0) :: dedent ::: (createToken(NEWLINE, 0) :: Nil)
        else
          createToken(EOF, 0) :: dedent

        eof ::: tokens
      case c :: _                           =>
        val (token, tail) = endInvalidToken(chars, 0, isEndingChar, length => report(InvalidIdentifier(c, length)))
        readTokens(tail, token :: tokens)
    }

    val characters = stripReturnCarriage(source.text).toList
    val res = readTokens(characters, Nil).reverse

    line = 1
    column = 1
    res
  }

  private def stripReturnCarriage(text: String) = {
    if (System.lineSeparator().contains('\r')) text.replaceAll("\r", "")
    else text
  }

  private def parseCharacters(chars: List[Char], char: Char): (Int, List[Char]) = {
    @tailrec def numCharacters(chars: List[Char], numChars: Int): (Int, List[Char]) = chars match {
      case `char` :: rest => numCharacters(rest, numChars + 1)
      case _              => (numChars, chars)
    }
    numCharacters(chars, 0)
  }

  private def getIndentToken(chars: List[Char]): (List[Token], List[Char]) = {
    val (newIndent, parsedChars, rest) = indent(chars)

    val difference = newIndent - indent
    if (difference > 1)
      report(IndentationTooLong(indent, newIndent, parsedChars))

    val tokens = if (difference == 1)
      createToken(INDENT, newIndent) :: Nil
    else if (difference < 0)
      (0 until -difference).map(_ => createToken(DEDENT, newIndent)).toList
    else {
      Nil
    }
    column = 1 + parsedChars
    indent = newIndent
    (tokens, rest)
  }

  private def indent(chars: List[Char]): (Int, Int, List[Char]) = {
    var foundSpace = false
    var mixedTabsAndSpaces = false

    @tailrec def indent(chars: List[Char], currentIndent: Int, parsedChars: Int): (Int, Int, List[Char]) = chars match {
      case '\t' :: rest =>
        if (foundSpace)
          mixedTabsAndSpaces = true
        indent(rest, currentIndent + 1, parsedChars + 1)
      case ' ' :: rest  =>
        foundSpace = true
        indent(rest, currentIndent, parsedChars + 1)
      case '\n' :: _    =>
        report(UnnecessaryWhitespaceOnBlankLine(parsedChars))
        (currentIndent, parsedChars, chars)
      case _            => (currentIndent, parsedChars, chars)
    }
    indent(chars, 0, 0) use { case (_, parsedChars, _) =>
      if (mixedTabsAndSpaces)
        report(IndentationMixesTabsAndSpaces(parsedChars))
    }

  }

  private def tokenExists(str: String): Boolean = NonKeywords.contains(str)

  private def getIdentifierOrKeyword(chars: List[Char]): (Token, List[Char]) = {

    def createIdentifierOrKeyWord(s: String): Token = KeywordMap.get(s) match {
      case Some(keyword) => createToken(keyword, s.length)
      case None          => createIdToken(s, s.length)
    }

    @tailrec def getIdentifierOrKeyword(chars: List[Char], s: StringBuilder, parsed: Int): (Token, List[Char]) = {
      def validChar(c: Char) = c.isLetter || c.isDigit || c == '_'

      chars match {
        case c :: r if validChar(c)    => getIdentifierOrKeyword(r, s + c, parsed + 1)
        case c :: _ if isEndingChar(c) => (createIdentifierOrKeyWord(s.toString), chars)
        case c :: r                    =>
          endInvalidToken(r, parsed, isEndingChar, length => report(InvalidIdentifier(c, length)))
        case Nil                       => (createIdentifierOrKeyWord(s.toString), chars)
      }
    }

    getIdentifierOrKeyword(chars, new StringBuilder, 1)
  }


  private def getCharLiteral(chars: List[Char]): (Token, List[Char]) = {

    @tailrec def toEnd(charList: List[Char], parsed: Int): (Token, List[Char]) =
      charList match {
        case Nil       =>
          report(UnclosedCharLiteral(parsed))
          (createToken(BAD, parsed), Nil)
        case '\'' :: r =>
          report(InvalidCharLiteral(parsed))
          (createToken(BAD, parsed), r)
        case _ :: r    =>
          toEnd(r, parsed + 1)
      }

    chars match {
      case '\'' :: r =>
        report(EmptyCharLiteral())
        (createToken(BAD, 2), r)

      // Unicodes
      case '\\' :: 'u' :: r => r match {
        case c1 :: c2 :: c3 :: c4 :: '\'' :: r if areHexDigits(c1, c2, c3, c4) =>
          val unicodeNumber = Integer.parseInt("" + c1 + c2 + c3 + c4, 16)
          (createToken(unicodeNumber.toChar, 8), r)
        case _ :: _ :: _ :: _ :: '\'' :: r                                     =>
          report(InvalidUnicode(8))
          (createToken(BAD, 8), r)
        case _ :: _ :: _ :: '\'' :: r                                          =>
          report(InvalidUnicode(7))
          (createToken(BAD, 7), r)
        case _ :: _ :: '\'' :: r                                               =>
          report(InvalidUnicode(6))
          (createToken(BAD, 6), r)
        case _ :: '\'' :: r                                                    =>
          report(InvalidUnicode(5))
          (createToken(BAD, 5), r)
        case '\'' :: r                                                         =>
          report(InvalidUnicode(4))
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
        case '\\' => (createToken('\\', 4), r)
        case _    =>
          report(InvalidEscapeSequence(4))
          (createToken(BAD, 4), r)
      }
      case '\\' :: '\'' :: r      =>
        report(InvalidCharLiteral(3))
        (createToken(BAD, 4), r)
      case c :: '\'' :: r         =>
        (createToken(c, 3), r)
      case r                      =>
        toEnd(r, 1)
    }
  }

  private def getStringLiteral(chars: List[Char]): (Token, List[Char]) = {

    val startPos = getStartPos

    def endAt(c: Char) = c == '"'

    @tailrec def getStringLiteral(chars: List[Char], s: StringBuilder, parsed: Int): (Token, List[Char]) = chars match {
      case '"' :: r  => (createToken(s.toString, parsed + 1), r)
      case '\n' :: _ =>
        report(UnclosedStringLiteral(parsed))
        (startPos, chars)
      case '\\' :: r => r match {
        case 't' :: r  => getStringLiteral(r, s + '\t', parsed + 2)
        case 'b' :: r  => getStringLiteral(r, s + '\b', parsed + 2)
        case 'n' :: r  => getStringLiteral(r, s + '\n', parsed + 2)
        case 'r' :: r  => getStringLiteral(r, s + '\r', parsed + 2)
        case 'f' :: r  => getStringLiteral(r, s + '\f', parsed + 2)
        case ''' :: r  => getStringLiteral(r, s + '\'', parsed + 2)
        case '"' :: r  => getStringLiteral(r, s + '\"', parsed + 2)
        case '\\' :: r => getStringLiteral(r, s + '\\', parsed + 2)
        case '[' :: r  => getStringLiteral(r, s + 27.toChar + '[', parsed + 2)
        case 'u' :: r  => r match {
          case c1 :: c2 :: c3 :: c4 :: r if areHexDigits(c1, c2, c3, c4) =>
            val unicodeNumber = Integer.parseInt("" + c1 + c2 + c3 + c4, 16)
            getStringLiteral(r, s + unicodeNumber.toChar, parsed + 6)
          case _                                                         =>
            endInvalidToken(chars, parsed, endAt, len => report(InvalidUnicode(len)))
        }
        case _         =>
          endInvalidToken(chars, parsed, endAt, len => report(InvalidEscapeSequence(len)))
      }
      case c :: r    => getStringLiteral(r, s + c, parsed + 1)
      case Nil       =>
        report(UnclosedStringLiteral(parsed))
        (startPos, chars)
    }

    getStringLiteral(chars, new StringBuilder, 1)
  }

  private def getMultiLineStringLiteral(chars: List[Char]): (Token, List[Char]) = {
    val startPos = getStartPos

    @tailrec def getStringIdentifier(chars: List[Char], s: StringBuilder): (Token, List[Char]) = chars match {
      case '`' :: r  =>
        column += 1
        val token = new STRLIT(s.toString)
        token.setPos(source, startPos.line, startPos.col, line, column)
        (token, r)
      case '\n' :: r =>
        line += 1
        column = 1
        getStringIdentifier(r, s + '\n')
      case c :: r    =>
        column += 1
        getStringIdentifier(r, s + c)
      case Nil       =>
        report(UnclosedMultilineString(startPos))
        (startPos, chars)
    }

    getStringIdentifier(chars, new StringBuilder)
  }

  private def getHexadecimalLiteral(chars: List[Char]) = {
    @tailrec def getHexadecimalLiteral(chars: List[Char], s: StringBuilder, parsed: Int): (Token, List[Char]) = chars match {
      case '_' :: r                  => getHexadecimalLiteral(r, s, parsed + 1)
      case c :: r if isHexDigit(c)   => getHexadecimalLiteral(r, s + c, parsed + 1)
      case ('l' | 'L') :: r          => (parseLongToken(s.toString, parsed + 1), r)
      case c :: _ if isEndingChar(c) => (parseIntToken(s.toString, parsed), chars)
      case Nil                       => (parseIntToken(s.toString, parsed), chars)
      case _                         => endInvalidToken(chars, parsed, _.isWhitespace, len => report(InvalidHexadecimalLiteral(len)))
    }

    if (chars.isEmpty || isEndingChar(chars.head))
      endInvalidToken(chars, 2, _.isWhitespace, len => report(InvalidHexadecimalLiteral(len)))
    else
      getHexadecimalLiteral(chars, new StringBuilder("0x"), 2)
  }

  private def getBinaryLiteral(chars: List[Char]) = {

    @tailrec def getBinaryLiteral(chars: List[Char], s: StringBuilder, parsed: Int): (Token, List[Char]) = chars match {
      case '_' :: r                  => getBinaryLiteral(r, s, parsed + 1)
      case '0' :: r                  => getBinaryLiteral(r, s + '0', parsed + 1)
      case '1' :: r                  => getBinaryLiteral(r, s + '1', parsed + 1)
      case ('l' | 'L') :: r          => (parseLongToken(s.toString, parsed + 1), r)
      case c :: _ if isEndingChar(c) => (parseIntToken(s.toString, parsed), chars)
      case Nil                       => (parseIntToken(s.toString, parsed), chars)
      case _                         => endInvalidToken(chars, parsed, _.isWhitespace, len => report(InvalidBinaryLiteral(len)))
    }

    if (chars.isEmpty || isEndingChar(chars.head))
      endInvalidToken(chars, 2, _.isWhitespace, len => report(InvalidBinaryLiteral(len)))
    else
      getBinaryLiteral(chars, new StringBuilder("0b"), 2)
  }

  private def getNumberLiteral(chars: List[Char]): (Token, List[Char]) = {
    var foundDecimal = false
    var foundE = false

    @tailrec def getNumberLiteral(chars: List[Char], s: String, parsed: Int): (Token, List[Char]) =
      chars match {
        case '_' :: r            => getNumberLiteral(r, s, parsed + 1)
        case c :: r if c.isDigit => getNumberLiteral(r, s + c, parsed + 1)
        case '.' :: r            =>
          if (!foundDecimal && !foundE) {
            foundDecimal = true
            r match {
              case c :: r if c.isDigit => getNumberLiteral(r, s + "." + c, parsed + 2)
              case _                   => (parseIntToken(s, parsed), chars)
            }
          } else {
            endInvalidToken(chars, parsed, _.isWhitespace, len => report(InvalidNumber(len)))
          }
        case ('e' | 'E') :: r    =>
          if (!foundE) {
            foundE = true
            r match {
              case '-' :: c :: r if c.isDigit => getNumberLiteral(r, s + "E-" + c, parsed + 3)
              case c :: r if c.isDigit        => getNumberLiteral(r, s + "E" + c, parsed + 2)
              case _                          =>
                endInvalidToken(chars, parsed, _.isWhitespace, len => report(InvalidFloat(len)))
            }
          } else {
            endInvalidToken(chars, parsed, _.isWhitespace, len => report(InvalidFloat(len)))
          }
        case ('f' | 'F') :: r    => (parseFloatToken(s, parsed + 1), r)
        case ('l' | 'L') :: r    =>
          if (!foundE && !foundDecimal) {
            (parseLongToken(s, parsed + 1), r)
          } else {
            endInvalidToken(chars, parsed, _.isWhitespace, len => report(InvalidFloat(len)))
          }
        case r                   =>
          if (foundDecimal || foundE) {
            (parseDoubleToken(s, parsed), r)
          } else {
            val rest = if (foundDecimal) '.' :: r else r
            (parseIntToken(s, parsed), rest)
          }
      }

    getNumberLiteral(chars, "", 0)
  }

  private def lineComment(chars: List[Char]): (Token, List[Char]) = {
    @tailrec def lineComment(chars: List[Char], s: StringBuilder): (Token, List[Char]) = chars match {
      case '\n' :: r => (createCommentToken(s.toString, s.length), '\n' :: r)
      case c :: r    => lineComment(r, s + c)
      case Nil       => (createCommentToken(s.toString, s.length), chars)
    }

    lineComment(chars, new StringBuilder("//"))

  }

  private def blockComment(chars: List[Char]): (Token, List[Char]) = {
    val startPos = getStartPos

    @tailrec def blockComment(chars: List[Char], s: StringBuilder): (Token, List[Char]) = chars match {
      case '*' :: '/' :: r =>
        column += 2
        val token = new COMMENTLIT(s.toString + "*/")
        token.setPos(source, startPos.line, startPos.col, line, column)
        (token, r)
      case '\n' :: r       =>
        line += 1
        column = 1
        blockComment(r, s + '\\' + 'n')
      case c :: r          =>
        column += 1
        blockComment(r, s + c)
      case Nil             =>
        val token = new COMMENTLIT(s.toString)
        token.setPos(source, startPos.line, startPos.col, line, column)
        (token, Nil)
    }

    column += 2
    blockComment(chars, new StringBuilder("/*"))
  }

  private def endInvalidToken(
    chars: List[Char],
    parsed: Int,
    endAt: Char => Boolean,
    error: Int => Unit): (Token, List[Char]) = {

    @tailrec def toEnd(chars: List[Char], parsed: Int): (Token, List[Char]) =
      chars match {
        case Nil                =>
          error(parsed)
          (createToken(BAD, parsed), Nil)
        case c :: r if endAt(c) =>
          error(parsed)
          (createToken(BAD, parsed), r)
        case _ :: r             =>
          toEnd(r, parsed + 1)
      }

    toEnd(chars, parsed)
  }

  private def parseIntToken(numStr: String, len: Int): Token = {
    def invalid(len: Int) = {
      report(NumberTooLargeForInt(len))
      createToken(BAD, len)
    }

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

  private def getStartPos = createToken(BAD, 0)


  private def parseLongToken(numStr: String, len: Int): Token = {
    def invalid(len: Int) = {
      report(NumberTooLargeForLong(len))
      createToken(BAD, len)
    }

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
  private def parseFloatToken(numStr: String, len: Int): Token = createToken(numStr.toFloat, len)

  private def parseDoubleToken(numStr: String, len: Int): Token = createToken(numStr.toDouble, len)

  private def isEndingChar(c: Char) = c.isWhitespace || NonKeywords.contains(s"$c")

  private def areHexDigits(chars: Char*) = chars forall isHexDigit

  private def isHexDigit(c: Char) = c.isDigit || "abcdef".contains(c.toLower)

  private def createToken(int: Int, tokenLength: Int): Token = createToken(new INTLIT(int), tokenLength)

  private def createToken(long: Long, tokenLength: Int): Token = createToken(new LONGLIT(long), tokenLength)

  private def createToken(float: Float, tokenLength: Int): Token = createToken(new FLOATLIT(float), tokenLength)

  private def createToken(double: Double, tokenLength: Int): Token = createToken(new DOUBLELIT(double), tokenLength)

  private def createToken(kind: TokenKind, tokenLength: Int): Token = createToken(new Token(kind), tokenLength)

  private def createIdToken(string: String, tokenLength: Int): Token = createToken(new ID(string), tokenLength)

  private def createCommentToken(str: String, tokenLength: Int): Token = createToken(new COMMENTLIT(str), tokenLength)

  private def createToken(char: Char, tokenLength: Int): Token = createToken(new CHARLIT(char), tokenLength)

  private def createToken(string: String, tokenLength: Int): Token = createToken(new STRLIT(string), tokenLength)

  private def createToken(token: Token, tokenLength: Int): Token = {
    token.setPos(source, line, column, line, column + tokenLength)
    column += tokenLength
    token
  }
}