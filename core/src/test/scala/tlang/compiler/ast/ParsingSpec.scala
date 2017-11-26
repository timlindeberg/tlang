package tlang.compiler.ast

import tlang.Context
import tlang.compiler.DebugOutputFormatter
import tlang.compiler.ast.Trees._
import tlang.compiler.lexer.Tokens._
import tlang.compiler.lexer.{Token, TokenKind}
import tlang.messages.{ErrorStringContext, Reporter}
import tlang.testutils.UnitSpec

class ParsingSpec extends UnitSpec {

  behavior of "A parser"

  // -- Declarations

  it should "parse a compilation unit" in { pending }
  it should "create a main class" in { pending }
  it should "parse a package declaration" in { pending }
  it should "parse a import declaration" in { pending }
  it should "parse an extension import" in { pending }
  it should "parse a class declaration" in { pending }
  it should "parse a trait declaration" in { pending }
  it should "parse an extension declaration" in { pending }
  it should "parse a variable declaration" in { pending }
  it should "parse a method declaration" in { pending }
  it should "parse a constructor declaration" in { pending }
  it should "parse operator declarations" in { pending }

  // -- Statements

  it should "parse blocks" in {
    parser(
      INDENT,
      CONTINUE,
      NEWLINE,
      BREAK,
      NEWLINE,
      DEDENT
    ).statement shouldBe Block(List(Continue(), Break()))
  }

  it should "parse if statements" in {
    test("If only without else") {
      parser(
        IF,
        LPAREN,
        TRUE,
        RPAREN,
        CONTINUE
      ).statement shouldBe If(TrueLit(), Continue(), None)
    }

    test("If else on four lines") {
      parser(
        IF,
        LPAREN,
        FALSE,
        RPAREN,
        NEWLINE,
        CONTINUE,
        NEWLINE,
        ELSE,
        NEWLINE,
        BREAK
      ).statement shouldBe If(FalseLit(), Continue(), Some(Break()))
    }

    test("If else on two lines") {
      parser(
        IF,
        LPAREN,
        FALSE,
        RPAREN,
        CONTINUE,
        NEWLINE,
        ELSE,
        BREAK
      ).statement shouldBe If(FalseLit(), Continue(), Some(Break()))
    }

    test("If else on one line") {
      parser(
        IF,
        LPAREN,
        FALSE,
        RPAREN,
        CONTINUE,
        ELSE,
        BREAK
      ).statement shouldBe If(FalseLit(), Continue(), Some(Break()))
    }


  }
  it should "parse while loops" in { pending }
  it should "parse for loops" in { pending }
  it should "parse for each loops" in { pending }
  it should "parse print and error statements statements" in { pending }
  it should "parse return statements" in { pending }
  it should "parse break and continue statements" in { pending }
  it should "end statements at semicolon or newline" in { pending }

  // -- Expressions

  it should "parse assignments" in { pending }
  it should "parse operator assignments" in { pending }
  it should "parse ternary expressions" in { pending }
  it should "parse or expressions" in { pending }
  it should "parse and expressions" in { pending }
  it should "parse logicOr expressions" in { pending }
  it should "parse logicXor expressions" in { pending }
  it should "parse logicAnd expressions" in { pending }
  it should "parse equals and not equals expressions" in { pending }
  it should "parse is expressions" in { pending }
  it should "parse comparison expressions" in { pending }
  it should "parse bitshift expressions" in { pending }
  it should "parse plus and minus expressions" in { pending }
  it should "parse times and division expressions" in { pending }
  it should "parse operators with the correct precedence" in { pending }

  // -- Terms

  it should "parse expressions with parentheses with the correct precedence" in { pending }
  it should "parse array literals" in { pending }
  it should "parse not expressions" in { pending }
  it should "parse unary minus expressions" in { pending }
  it should "parse hash expressions" in { pending }
  it should "parse pre increment expressions" in { pending }
  it should "parse pre decrement expressions" in { pending }
  it should "parse literals" in { pending }
  it should "parse identifiers" in { pending }
  it should "parse true, false this and null expressions" in { pending }
  it should "parse super expressions" in { pending }
  it should "parse new expressions" in { pending }
  it should "parse access expressions" in { pending }
  it should "parse indexing expressions" in { pending }
  it should "parse slicing expressions" in { pending }
  it should "parse as expressions" in { pending }
  it should "parse post increment expressions" in { pending }
  it should "parse post decrement expressions" in { pending }
  it should "parse extract nullable expressions" in { pending }
  it should "parse expressions with multiple " in { pending }


  // -- Misc

  it should "parse types" in { pending }
  it should "parse a formal" in { pending }
  it should "replace the last expression with a return statement" in { pending }
  it should "parse class type identifiers with recursive template types" in { pending }

  private def parser(tokens: Any*) = {
    val tokenStream = TokenStream(tokens.map {
      case t: Token        => t
      case kind: TokenKind => new Token(kind)
      case _               => ???
    })
    val ctx = Context(mock[Reporter], createMockFormatter(), mock[DebugOutputFormatter])
    Parser(ctx, mock[ErrorStringContext], tokenStream)
  }

}
