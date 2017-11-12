package tlang.compiler.lexer

import tlang.compiler.lexer.Tokens._
import tlang.messages.{ErrorStringContext, Reporter}
import tlang.testutils.UnitSpec
import tlang.utils.Extensions._
import tlang.utils.StringSource

class LexingSpec extends UnitSpec {

  behavior of "A lexer"


  it should "should parse key words" in {
    lex("val Var def protected static this implicit null") should contain inOrder(
      new Token(PRIVVAL),
      new Token(PUBVAR),
      new Token(PRIVDEF),
      new Token(PROTECTED),
      new Token(STATIC),
      new Token(THIS),
      new Token(IMPLICIT),
      new Token(NULL)
    )
  }


  it should "should parse symbols" in {
    lex("+ += -- ++ <<= ^= . ; !! ?: ? ?.") should contain inOrder(
      new Token(PLUS),
      new Token(PLUSEQ),
      new Token(DECREMENT),
      new Token(INCREMENT),
      new Token(LEFTSHIFTEQ),
      new Token(XOREQ),
      new Token(DOT),
      new Token(SEMICOLON),
      new Token(EXTRACTNULLABLE),
      new Token(ELVIS),
      new Token(QUESTIONMARK),
      new Token(SAFEACCESS)
    )
  }


  it should "should parse identifiers" in {
    lex("abc _ABC ___ a123 _123 statica _return") should contain inOrder(
      ID("abc"),
      ID("_ABC"),
      ID("___"),
      ID("a123"),
      ID("_123"),
      ID("statica"),
      ID("_return")
    )
  }


  it should "should parse whitespace tokens" in {
    lex(s"$NL$NL\t$NL\t\t$NL") should contain theSameElementsInOrderAs Seq(
      new Token(NEWLINE),
      new Token(INDENT),
      new Token(NEWLINE),
      new Token(INDENT),
      new Token(NEWLINE),
      new Token(DEDENT),
      new Token(DEDENT),
      new Token(EOF)
    )
  }


  it should "should parse char literals" in {
    lex("""'a' '\n' '\t' '\u001b' '\''""") should contain inOrder(
      CHARLIT('a'),
      CHARLIT('\n'),
      CHARLIT('\t'),
      CHARLIT('\u001b'),
      CHARLIT('\'')
    )
  }


  it should "should parse string literals" in {
    lex(""" "abc" "a \t" "\n" "\u001b 55 \u001b[31m" """) should contain inOrder(
      STRLIT("abc"),
      STRLIT("a \t"),
      STRLIT("\n"),
      STRLIT("\u001b 55 \u001b[31m")
    )
  }


  it should "should parse multi line string literals" in {
    lex(
      """|` abcde ad  \n
         |ads \r das \t
         |as`
      """.stripMargin
    ) should contain(
      STRLIT(s" abcde ad  \\n${ NL }ads \\r das \\t${ NL }as")
    )
  }


  it should "should parse hexadecimal number literals" in {
    lex("0x0 0x1 0x1A 0x123456789AL 0xabCdeF 0x7FFFFFFFFFFFFFFFl") should contain inOrder(
      INTLIT(0),
      INTLIT(1),
      INTLIT(26),
      LONGLIT(78187493530L),
      INTLIT(11259375),
      LONGLIT(9223372036854775807L)
    )
  }


  it should "should parse binary number literals" in {
    lex("0b0 0b1 0b01010101 0b1111111111111111111111111111111111l") should contain inOrder(
      INTLIT(0),
      INTLIT(1),
      INTLIT(85),
      LONGLIT(17179869183L)
    )
  }


  it should "should parse number literals" in {
    lex("123 12f 1.0 1e4 5E-1F 123_456_789_0123L") should contain inOrder(
      INTLIT(123),
      FLOATLIT(12f),
      DOUBLELIT(1.0),
      DOUBLELIT(10000.0),
      FLOATLIT(0.5f),
      LONGLIT(1234567890123L)
    )
  }


  it should "should parse line comments" in {
    lex(
      """|123 //  123 abc ef gh
         |// abc 123
      """.stripMargin) should contain allOf(
      COMMENTLIT("//  123 abc ef gh"),
      COMMENTLIT("// abc 123")
    )
  }


  it should "should parse block comments" in {
    lex(
      s"""|abc /* abcdeg sad13
          | fdad dsa
          |abc \t\t */
       """.stripMargin) should contain(
      COMMENTLIT(s"/* abcdeg sad13$NL fdad dsa${ NL }abc \t\t */")
    )
  }

  private def lex(s: String) = newLexer(StringSource(s, ""))
  private def newLexer = Lexer(mock[Reporter], mock[ErrorStringContext])

}
