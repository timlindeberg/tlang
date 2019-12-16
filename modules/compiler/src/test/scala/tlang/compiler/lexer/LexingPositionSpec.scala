package tlang
package compiler
package lexer

import better.files.File
import org.scalatest.AppendedClues
import tlang.compiler.lexer.Tokens._
import tlang.testutils.TestConstants._
import tlang.utils.{FileSource, Position}
import tlang.compiler.testutils.PositionTest

class LexingPositionSpec extends CompilerIntegrationTestSpec with AppendedClues {

  val TestFile: File = File(s"$Resources/positions/LexerPositions.t")

  lazy val Tokens: List[Token] = {
    val file = FileSource(TestFile) :: Nil
    Lexing.execute(TestContext)(file).head
  }

  def testPositions(predicate: Token => Boolean, tests: PositionTest*): Unit = {
    val tokens = Tokens
      .filter(_.kind != NEWLINE)
      .filter(predicate)
      .map(Position(_))

    tokens
      .zip(tests)
      .foreach { case (foundPos, test) => PositionTest.compare(foundPos, test) }
  }

  "Basic Tokens" in {
    testPositions(_.kind.str.length > 0,
      PositionTest(1, 1, 1, 2, ";"),
      PositionTest(1, 3, 1, 4, "."),
      PositionTest(1, 5, 1, 6, ":"),
      PositionTest(2, 1, 2, 2, ","),
      PositionTest(2, 3, 2, 4, "="),
      PositionTest(2, 5, 2, 7, "+="),
      PositionTest(3, 1, 3, 3, "-="),
      PositionTest(3, 4, 3, 6, "*="),
      PositionTest(3, 7, 3, 9, "/="),
      PositionTest(4, 1, 4, 3, "%="),
      PositionTest(4, 4, 4, 6, "&="),
      PositionTest(4, 7, 4, 9, "|="),
      PositionTest(5, 1, 5, 3, "^="),
      PositionTest(5, 4, 5, 7, "<<="),
      PositionTest(5, 8, 5, 11, ">>="),
      PositionTest(6, 1, 6, 3, "=="),
      PositionTest(6, 4, 6, 6, "!="),
      PositionTest(6, 7, 6, 8, "!"),
      PositionTest(7, 1, 7, 2, "#"),
      PositionTest(7, 3, 7, 5, "++"),
      PositionTest(7, 6, 7, 8, "--"),
      PositionTest(8, 1, 8, 2, "("),
      PositionTest(8, 2, 8, 3, ")"),
      PositionTest(8, 3, 8, 4, "["),
      PositionTest(9, 1, 9, 2, "]"),
      PositionTest(9, 2, 9, 3, "{"),
      PositionTest(9, 3, 9, 4, "}"),
      PositionTest(10, 1, 10, 3, "&&"),
      PositionTest(10, 3, 10, 5, "||"),
      PositionTest(10, 5, 10, 6, "?"),
      PositionTest(11, 1, 11, 3, "?."),
      PositionTest(11, 4, 11, 6, "?:"),
      PositionTest(11, 7, 11, 8, "~"),
      PositionTest(12, 1, 12, 2, "&"),
      PositionTest(12, 3, 12, 4, "|"),
      PositionTest(12, 5, 12, 6, "^"),
      PositionTest(13, 1, 13, 2, "%"),
      PositionTest(13, 3, 13, 5, "<<"),
      PositionTest(13, 6, 13, 8, ">>"),
      PositionTest(14, 1, 14, 2, "<"),
      PositionTest(14, 3, 14, 5, "<="),
      PositionTest(14, 6, 14, 7, ">"),
      PositionTest(15, 1, 15, 3, ">="),
      PositionTest(15, 4, 15, 5, "+"),
      PositionTest(15, 6, 15, 7, "-"),
      PositionTest(16, 1, 16, 2, "*"),
      PositionTest(16, 3, 16, 4, "/"),
      PositionTest(16, 5, 16, 7, "!!"),
      PositionTest(17, 1, 17, 8, "package"),
      PositionTest(17, 9, 17, 15, "import"),
      PositionTest(17, 16, 17, 18, "is"),
      PositionTest(18, 1, 18, 3, "as"),
      PositionTest(18, 4, 18, 9, "class"),
      PositionTest(19, 1, 19, 10, "extension"),
      PositionTest(19, 11, 19, 16, "trait"),
      PositionTest(19, 17, 19, 20, "Def"),
      PositionTest(20, 1, 20, 4, "def"),
      PositionTest(20, 5, 20, 14, "protected"),
      PositionTest(20, 15, 20, 18, "Var"),
      PositionTest(21, 1, 21, 4, "Val"),
      PositionTest(21, 5, 21, 8, "var"),
      PositionTest(21, 9, 21, 12, "val"),
      PositionTest(22, 1, 22, 7, "static"),
      PositionTest(22, 8, 22, 13, "while"),
      PositionTest(22, 14, 22, 17, "for"),
      PositionTest(23, 1, 23, 3, "if"),
      PositionTest(23, 4, 23, 8, "else"),
      PositionTest(23, 9, 23, 15, "return"),
      PositionTest(24, 1, 24, 5, "true"),
      PositionTest(24, 6, 24, 11, "false"),
      PositionTest(24, 12, 24, 16, "this"),
      PositionTest(25, 1, 25, 6, "super"),
      PositionTest(25, 7, 25, 10, "new"),
      PositionTest(25, 11, 25, 19, "implicit"),
      PositionTest(27, 1, 27, 6, "print"),
      PositionTest(27, 7, 27, 14, "println"),
      PositionTest(27, 15, 27, 20, "error"),
      PositionTest(29, 5, 29, 10, "break"),
      PositionTest(29, 19, 29, 27, "continue"),
      PositionTest(29, 37, 29, 39, "in"),
      PositionTest(31, 1, 31, 5, "null")
    )
  }

  "Identifiers" in {
    testPositions(_.kind == IDKIND,
      PositionTest(33, 1, 33, 11, "identifier"),
      PositionTest(33, 12, 33, 13, "x"),
      PositionTest(33, 14, 33, 17, "hej"),
      PositionTest(33, 23, 33, 27, "test"),
      PositionTest(33, 28, 33, 30, "id"),
      PositionTest(67, 1, 67, 2, "a"),
      PositionTest(68, 2, 68, 3, "b"),
      PositionTest(69, 3, 69, 4, "c"),
      PositionTest(70, 8, 70, 9, "d"),
      PositionTest(70, 17, 70, 18, "e"),
      PositionTest(71, 3, 71, 4, "f")
    )
  }

  "Number literals" in {
    testPositions(_.kind in List(INTLITKIND, LONGLITKIND, FLOATLITKIND, DOUBLELITKIND),
      PositionTest(38, 6, 38, 7, "0"),
      PositionTest(39, 6, 39, 15, "123456789"),
      PositionTest(40, 6, 40, 15, "1_234_567"),
      PositionTest(41, 6, 41, 8, "0L"),
      PositionTest(42, 6, 42, 12, "12345l"),
      PositionTest(43, 6, 43, 14, "0xabcdef"),
      PositionTest(44, 6, 44, 14, "0xabcdel"),
      PositionTest(45, 6, 45, 17, "0x123abcdeL"),
      PositionTest(46, 6, 46, 14, "0b010101"),
      PositionTest(47, 6, 47, 15, "0b010101L"),
      PositionTest(48, 6, 48, 11, "0.000"),
      PositionTest(49, 6, 49, 12, "0.000f"),
      PositionTest(50, 6, 50, 11, "0.01F"),
      PositionTest(51, 6, 51, 13, "0.1e-5F"),
      PositionTest(52, 6, 52, 14, "0.1E123F")
    )
  }

  "Char Literals" in {
    testPositions(_.kind == CHARLITKIND,
      PositionTest(55, 6, 55, 9, "'a'"),
      PositionTest(56, 6, 56, 10, "'\\n'"),
      PositionTest(57, 6, 57, 14, "'\\uabcd'")
    )
  }

  "String literals" in {
    testPositions(_.kind == STRLITKIND,
      PositionTest(59, 6, 59, 8, "\"\""),
      PositionTest(60, 6, 60, 14, "\"string\""),
      PositionTest(61, 6, 61, 24, "\"stri\\ng\\uabcdhej\""),
      PositionTest(62, 6, 65, 7, "eine kleine multilinen stringen")
    )
  }

  "Comment literals" in {
    testPositions(_.kind == COMMENTLITKIND,
      PositionTest(29, 1, 29, 5, "/**/"),
      PositionTest(29, 12, 29, 18, "/*  */"),
      PositionTest(29, 28, 29, 36, "/******/"),
      PositionTest(29, 40, 29, 46, "/** */"),
      PositionTest(30, 1, 30, 8, "// null"),
      PositionTest(31, 6, 32, 22, "/* null  \\n null  */")
    )
  }

  "Indents" in {
    testPositions(_.kind == INDENT,
      PositionTest(68, 1, 68, 2, "a->b"),
      PositionTest(69, 1, 69, 3, "b->->c"),
      PositionTest(70, 1, 70, 4, "c->->->d"),
      PositionTest(73, 1, 73, 2, "g->h"),
      PositionTest(74, 1, 74, 3, "h->i")
    )
  }

  "Dedents" in {
    testPositions(_.kind == DEDENT,
      PositionTest(71, 1, 71, 3, "e<-<-f"),
      PositionTest(72, 1, 72, 1, "f<-<-g"),
      PositionTest(72, 1, 72, 1, "f<-<-g"),
      PositionTest(75, 1, 75, 1, "i<-EOF"),
      PositionTest(75, 1, 75, 1, "i<-EOF")
    )
  }
}
