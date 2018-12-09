package tlang
package compiler
package lexer

import better.files.File
import org.scalatest.AppendedClues
import tlang.compiler.{CompilerIntegrationTestSpec, Context}
import tlang.compiler.lexer.Tokens._
import tlang.testutils.TestConstants._

import tlang.utils.{FileSource, Position}

class LexingPositionSpec extends CompilerIntegrationTestSpec with AppendedClues {

  val TestFile: File = File(s"$Resources/positions/LexerPositions.t")

  lazy val Tokens: List[Token] = {
    val file = FileSource(TestFile) :: Nil
    Lexing.execute(TestContext)(file).head
  }

  def testPositions(predicate: Token => Boolean, positions: (Position, String)*): Unit = {
    val tokens = Tokens
      .filter(_.kind != NEWLINE)
      .filter(predicate)
      .map(Position(_))

    tokens.zip(positions) foreach { case (foundPos, (expectedPos, description)) =>
      foundPos shouldBe expectedPos withClue s"for token '$description'"
    }
  }

  "Basic Tokens" in {
    testPositions(_.kind.str.length > 0,
      (Position(1, 1, 1, 2), ";"),
      (Position(1, 3, 1, 4), "."),
      (Position(1, 5, 1, 6), ":"),
      (Position(2, 1, 2, 2), ","),
      (Position(2, 3, 2, 4), "="),
      (Position(2, 5, 2, 7), "+="),
      (Position(3, 1, 3, 3), "-="),
      (Position(3, 4, 3, 6), "*="),
      (Position(3, 7, 3, 9), "/="),
      (Position(4, 1, 4, 3), "%="),
      (Position(4, 4, 4, 6), "&="),
      (Position(4, 7, 4, 9), "|="),
      (Position(5, 1, 5, 3), "^="),
      (Position(5, 4, 5, 7), "<<="),
      (Position(5, 8, 5, 11), ">>="),
      (Position(6, 1, 6, 3), "=="),
      (Position(6, 4, 6, 6), "!="),
      (Position(6, 7, 6, 8), "!"),
      (Position(7, 1, 7, 2), "#"),
      (Position(7, 3, 7, 5), "++"),
      (Position(7, 6, 7, 8), "--"),
      (Position(8, 1, 8, 2), "("),
      (Position(8, 2, 8, 3), ")"),
      (Position(8, 3, 8, 4), "["),
      (Position(9, 1, 9, 2), "]"),
      (Position(9, 2, 9, 3), "{"),
      (Position(9, 3, 9, 4), "}"),
      (Position(10, 1, 10, 3), "&&"),
      (Position(10, 3, 10, 5), "||"),
      (Position(10, 5, 10, 6), "?"),
      (Position(11, 1, 11, 3), "?."),
      (Position(11, 4, 11, 6), "?:"),
      (Position(11, 7, 11, 8), "~"),
      (Position(12, 1, 12, 2), "&"),
      (Position(12, 3, 12, 4), "|"),
      (Position(12, 5, 12, 6), "^"),
      (Position(13, 1, 13, 2), "%"),
      (Position(13, 3, 13, 5), "<<"),
      (Position(13, 6, 13, 8), ">>"),
      (Position(14, 1, 14, 2), "<"),
      (Position(14, 3, 14, 5), "<="),
      (Position(14, 6, 14, 7), ">"),
      (Position(15, 1, 15, 3), ">="),
      (Position(15, 4, 15, 5), "+"),
      (Position(15, 6, 15, 7), "-"),
      (Position(16, 1, 16, 2), "*"),
      (Position(16, 3, 16, 4), "/"),
      (Position(16, 5, 16, 7), "!!"),
      (Position(17, 1, 17, 8), "package"),
      (Position(17, 9, 17, 15), "import"),
      (Position(17, 16, 17, 18), "is"),
      (Position(18, 1, 18, 3), "as"),
      (Position(18, 4, 18, 9), "class"),
      (Position(19, 1, 19, 10), "extension"),
      (Position(19, 11, 19, 16), "trait"),
      (Position(19, 17, 19, 20), "Def"),
      (Position(20, 1, 20, 4), "def"),
      (Position(20, 5, 20, 14), "protected"),
      (Position(20, 15, 20, 18), "Var"),
      (Position(21, 1, 21, 4), "Val"),
      (Position(21, 5, 21, 8), "var"),
      (Position(21, 9, 21, 12), "val"),
      (Position(22, 1, 22, 7), "static"),
      (Position(22, 8, 22, 13), "while"),
      (Position(22, 14, 22, 17), "for"),
      (Position(23, 1, 23, 3), "if"),
      (Position(23, 4, 23, 8), "else"),
      (Position(23, 9, 23, 15), "return"),
      (Position(24, 1, 24, 5), "true"),
      (Position(24, 6, 24, 11), "false"),
      (Position(24, 12, 24, 16), "this"),
      (Position(25, 1, 25, 6), "super"),
      (Position(25, 7, 25, 10), "new"),
      (Position(25, 11, 25, 19), "implicit"),
      (Position(27, 1, 27, 6), "print"),
      (Position(27, 7, 27, 14), "println"),
      (Position(27, 15, 27, 20), "error"),
      (Position(29, 5, 29, 10), "break"),
      (Position(29, 19, 29, 27), "continue"),
      (Position(29, 37, 29, 39), "in"),
      (Position(31, 1, 31, 5), "null")
    )
  }

  "Identifiers" in {
    testPositions(_.kind == IDKIND,
      (Position(33, 1, 33, 11), "identifier"),
      (Position(33, 12, 33, 13), "x"),
      (Position(33, 14, 33, 17), "hej"),
      (Position(33, 23, 33, 27), "test"),
      (Position(33, 28, 33, 30), "id"),
      (Position(67, 1, 67, 2), "a"),
      (Position(68, 2, 68, 3), "b"),
      (Position(69, 3, 69, 4), "c"),
      (Position(70, 8, 70, 9), "d"),
      (Position(70, 17, 70, 18), "e"),
      (Position(71, 3, 71, 4), "f")
    )
  }

  "Number literals" in {
    testPositions(_.kind in List(INTLITKIND, LONGLITKIND, FLOATLITKIND, DOUBLELITKIND),
      (Position(38, 6, 38, 7), "0"),
      (Position(39, 6, 39, 15), "123456789"),
      (Position(40, 6, 40, 15), "1_234_567"),
      (Position(41, 6, 41, 8), "0L"),
      (Position(42, 6, 42, 12), "12345l"),
      (Position(43, 6, 43, 14), "0xabcdef"),
      (Position(44, 6, 44, 14), "0xabcdel"),
      (Position(45, 6, 45, 17), "0x123abcdeL"),
      (Position(46, 6, 46, 14), "0b010101"),
      (Position(47, 6, 47, 15), "0b010101L"),
      (Position(48, 6, 48, 11), "0.000"),
      (Position(49, 6, 49, 12), "0.000f"),
      (Position(50, 6, 50, 11), "0.01F"),
      (Position(51, 6, 51, 13), "0.1e-5F"),
      (Position(52, 6, 52, 14), "0.1E123F")
    )
  }

  "Char Literals" in {
    testPositions(_.kind == CHARLITKIND,
      (Position(55, 6, 55, 9), "'a'"),
      (Position(56, 6, 56, 10), "'\\n'"),
      (Position(57, 6, 57, 14), "'\\uabcd'")
    )
  }

  "String literals" in {
    testPositions(_.kind == STRLITKIND,
      (Position(59, 6, 59, 8), "\"\""),
      (Position(60, 6, 60, 14), "\"string\""),
      (Position(61, 6, 61, 24), "\"stri\\ng\\uabcdhej\""),
      (Position(62, 6, 65, 7), "eine kleine multilinen stringen")
    )
  }

  "Comment literals" in {
    testPositions(_.kind == COMMENTLITKIND,
      (Position(29, 1, 29, 5), "/**/"),
      (Position(29, 12, 29, 18), "/*  */"),
      (Position(29, 28, 29, 36), "/******/"),
      (Position(29, 40, 29, 46), "/** */"),
      (Position(30, 1, 30, 8), "// null"),
      (Position(31, 6, 32, 22), "/* null  \\n null  */")
    )
  }

  "Indents" in {
    testPositions(_.kind == INDENT,
      (Position(68, 1, 68, 2), "a->b"),
      (Position(69, 1, 69, 3), "b->->c"),
      (Position(70, 1, 70, 4), "c->->->d")
    )
  }

  "Dedents" in {
    testPositions(_.kind == DEDENT,
      (Position(71, 1, 71, 3), "d<-<-e"),
      (Position(72, 1, 72, 1), "e<-<-EOF"),
      (Position(72, 1, 72, 1), "e<-EOF")
    )
  }
}
