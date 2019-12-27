package tlang
package compiler
package lexer

import better.files.File
import org.scalatest.AppendedClues
import tlang.compiler.lexer.Tokens._
import tlang.testutils.TestConstants._
import tlang.testutils.TestPosition
import tlang.utils.{FileSource, Position}

class LexingPositionSpec extends CompilerIntegrationTestSpec with AppendedClues {

  val TestFile: File = File(s"$Resources/positions/LexerPositions.t")

  lazy val Tokens: List[Token] = {
    val file = FileSource(TestFile) :: Nil
    Lexing.execute(TestContext)(file).head
  }

  def testPositions(predicate: Token => Boolean, tests: TestPosition*): Unit = {
    val tokens = Tokens
      .filter(_.kind != NEWLINE)
      .filter(predicate)
      .map(Position(_))

    tokens
      .zip(tests)
      .foreach { case (foundPos, test) => TestPosition.compare(foundPos, test) }
  }

  "Basic Tokens" in {
    testPositions(_.kind.str.length > 0,
      TestPosition(1, 1, 1, 2, ";"),
      TestPosition(1, 3, 1, 4, "."),
      TestPosition(1, 5, 1, 6, ":"),
      TestPosition(2, 1, 2, 2, ","),
      TestPosition(2, 3, 2, 4, "="),
      TestPosition(2, 5, 2, 7, "+="),
      TestPosition(3, 1, 3, 3, "-="),
      TestPosition(3, 4, 3, 6, "*="),
      TestPosition(3, 7, 3, 9, "/="),
      TestPosition(4, 1, 4, 3, "%="),
      TestPosition(4, 4, 4, 6, "&="),
      TestPosition(4, 7, 4, 9, "|="),
      TestPosition(5, 1, 5, 3, "^="),
      TestPosition(5, 4, 5, 7, "<<="),
      TestPosition(5, 8, 5, 11, ">>="),
      TestPosition(6, 1, 6, 3, "=="),
      TestPosition(6, 4, 6, 6, "!="),
      TestPosition(6, 7, 6, 8, "!"),
      TestPosition(7, 1, 7, 2, "#"),
      TestPosition(7, 3, 7, 5, "++"),
      TestPosition(7, 6, 7, 8, "--"),
      TestPosition(8, 1, 8, 2, "("),
      TestPosition(8, 2, 8, 3, ")"),
      TestPosition(8, 3, 8, 4, "["),
      TestPosition(9, 1, 9, 2, "]"),
      TestPosition(9, 2, 9, 3, "{"),
      TestPosition(9, 3, 9, 4, "}"),
      TestPosition(10, 1, 10, 3, "&&"),
      TestPosition(10, 3, 10, 5, "||"),
      TestPosition(10, 5, 10, 6, "?"),
      TestPosition(11, 1, 11, 3, "?."),
      TestPosition(11, 4, 11, 6, "?:"),
      TestPosition(11, 7, 11, 8, "~"),
      TestPosition(12, 1, 12, 2, "&"),
      TestPosition(12, 3, 12, 4, "|"),
      TestPosition(12, 5, 12, 6, "^"),
      TestPosition(13, 1, 13, 2, "%"),
      TestPosition(13, 3, 13, 5, "<<"),
      TestPosition(13, 6, 13, 8, ">>"),
      TestPosition(14, 1, 14, 2, "<"),
      TestPosition(14, 3, 14, 5, "<="),
      TestPosition(14, 6, 14, 7, ">"),
      TestPosition(15, 1, 15, 3, ">="),
      TestPosition(15, 4, 15, 5, "+"),
      TestPosition(15, 6, 15, 7, "-"),
      TestPosition(16, 1, 16, 2, "*"),
      TestPosition(16, 3, 16, 4, "/"),
      TestPosition(16, 5, 16, 7, "!!"),
      TestPosition(17, 1, 17, 8, "package"),
      TestPosition(17, 9, 17, 15, "import"),
      TestPosition(17, 16, 17, 18, "is"),
      TestPosition(18, 1, 18, 3, "as"),
      TestPosition(18, 4, 18, 9, "class"),
      TestPosition(19, 1, 19, 10, "extension"),
      TestPosition(19, 11, 19, 16, "trait"),
      TestPosition(19, 17, 19, 20, "Def"),
      TestPosition(20, 1, 20, 4, "def"),
      TestPosition(20, 5, 20, 14, "protected"),
      TestPosition(20, 15, 20, 18, "Var"),
      TestPosition(21, 1, 21, 4, "Val"),
      TestPosition(21, 5, 21, 8, "var"),
      TestPosition(21, 9, 21, 12, "val"),
      TestPosition(22, 1, 22, 7, "static"),
      TestPosition(22, 8, 22, 13, "while"),
      TestPosition(22, 14, 22, 17, "for"),
      TestPosition(23, 1, 23, 3, "if"),
      TestPosition(23, 4, 23, 8, "else"),
      TestPosition(23, 9, 23, 15, "return"),
      TestPosition(24, 1, 24, 5, "true"),
      TestPosition(24, 6, 24, 11, "false"),
      TestPosition(24, 12, 24, 16, "this"),
      TestPosition(25, 1, 25, 6, "super"),
      TestPosition(25, 7, 25, 10, "new"),
      TestPosition(25, 11, 25, 19, "implicit"),
      TestPosition(27, 1, 27, 6, "print"),
      TestPosition(27, 7, 27, 14, "println"),
      TestPosition(27, 15, 27, 20, "error"),
      TestPosition(29, 5, 29, 10, "break"),
      TestPosition(29, 19, 29, 27, "continue"),
      TestPosition(29, 37, 29, 39, "in"),
      TestPosition(31, 1, 31, 5, "null")
    )
  }

  "Identifiers" in {
    testPositions(_.kind == IDKIND,
      TestPosition(33, 1, 33, 11, "identifier"),
      TestPosition(33, 12, 33, 13, "x"),
      TestPosition(33, 14, 33, 17, "hej"),
      TestPosition(33, 23, 33, 27, "test"),
      TestPosition(33, 28, 33, 30, "id"),
      TestPosition(67, 1, 67, 2, "a"),
      TestPosition(68, 2, 68, 3, "b"),
      TestPosition(69, 3, 69, 4, "c"),
      TestPosition(70, 8, 70, 9, "d"),
      TestPosition(70, 17, 70, 18, "e"),
      TestPosition(71, 3, 71, 4, "f")
    )
  }

  "Number literals" in {
    testPositions(_.kind in List(INTLITKIND, LONGLITKIND, FLOATLITKIND, DOUBLELITKIND),
      TestPosition(38, 6, 38, 7, "0"),
      TestPosition(39, 6, 39, 15, "123456789"),
      TestPosition(40, 6, 40, 15, "1_234_567"),
      TestPosition(41, 6, 41, 8, "0L"),
      TestPosition(42, 6, 42, 12, "12345l"),
      TestPosition(43, 6, 43, 14, "0xabcdef"),
      TestPosition(44, 6, 44, 14, "0xabcdel"),
      TestPosition(45, 6, 45, 17, "0x123abcdeL"),
      TestPosition(46, 6, 46, 14, "0b010101"),
      TestPosition(47, 6, 47, 15, "0b010101L"),
      TestPosition(48, 6, 48, 11, "0.000"),
      TestPosition(49, 6, 49, 12, "0.000f"),
      TestPosition(50, 6, 50, 11, "0.01F"),
      TestPosition(51, 6, 51, 13, "0.1e-5F"),
      TestPosition(52, 6, 52, 14, "0.1E123F")
    )
  }

  "Char Literals" in {
    testPositions(_.kind == CHARLITKIND,
      TestPosition(55, 6, 55, 9, "'a'"),
      TestPosition(56, 6, 56, 10, "'\\n'"),
      TestPosition(57, 6, 57, 14, "'\\uabcd'")
    )
  }

  "String literals" in {
    testPositions(_.kind == STRLITKIND,
      TestPosition(59, 6, 59, 8, "\"\""),
      TestPosition(60, 6, 60, 14, "\"string\""),
      TestPosition(61, 6, 61, 24, "\"stri\\ng\\uabcdhej\""),
      TestPosition(62, 6, 65, 7, "eine kleine multilinen stringen")
    )
  }

  "Comment literals" in {
    testPositions(_.kind == COMMENTLITKIND,
      TestPosition(29, 1, 29, 5, "/**/"),
      TestPosition(29, 12, 29, 18, "/*  */"),
      TestPosition(29, 28, 29, 36, "/******/"),
      TestPosition(29, 40, 29, 46, "/** */"),
      TestPosition(30, 1, 30, 8, "// null"),
      TestPosition(31, 6, 32, 22, "/* null  \\n null  */")
    )
  }

  "Indents" in {
    testPositions(_.kind == INDENT,
      TestPosition(68, 1, 68, 2, "a->b"),
      TestPosition(69, 1, 69, 3, "b->->c"),
      TestPosition(70, 1, 70, 4, "c->->->d"),
      TestPosition(73, 1, 73, 2, "g->h"),
      TestPosition(74, 1, 74, 3, "h->i")
    )
  }

  "Dedents" in {
    testPositions(_.kind == DEDENT,
      TestPosition(71, 1, 71, 3, "e<-<-f"),
      TestPosition(72, 1, 72, 1, "f<-<-g"),
      TestPosition(72, 1, 72, 1, "f<-<-g"),
      TestPosition(75, 1, 75, 1, "i<-EOF"),
      TestPosition(75, 1, 75, 1, "i<-EOF")
    )
  }
}
