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


  //------------------------------------------------------------------------------------
  //--- Statements
  //------------------------------------------------------------------------------------

  it should "parse blocks" in {
    parser(INDENT, CONTINUE, NEWLINE, BREAK, NEWLINE, DEDENT).statement shouldBe Block(List(Continue(), Break()))
  }


  it should "parse if statements" in {
    test("If only without else") {
      parser(IF, LPAREN, TRUE, RPAREN, CONTINUE).statement shouldBe
        If(TrueLit(), Continue(), None)
    }

    test("If else on four lines") {
      parser(IF, LPAREN, FALSE, RPAREN, NEWLINE, CONTINUE, NEWLINE, ELSE, NEWLINE, BREAK).statement shouldBe
        If(FalseLit(), Continue(), Some(Break()))
    }

    test("If else on two lines") {
      parser(IF, LPAREN, FALSE, RPAREN, CONTINUE, NEWLINE, ELSE, BREAK).statement shouldBe
        If(FalseLit(), Continue(), Some(Break()))
    }

    test("If else on one line") {
      parser(IF, LPAREN, FALSE, RPAREN, CONTINUE, ELSE, BREAK).statement shouldBe
        If(FalseLit(), Continue(), Some(Break()))
    }
  }


  it should "parse while loops" in {
    parser(WHILE, LPAREN, FALSE, RPAREN, BREAK).statement shouldBe While(FalseLit(), Break())
  }


  it should "parse for loops" in {
    test("With everything") {
      parser(
        FOR,
        LPAREN,
        PRIVVAR,
        ID("i"),
        EQSIGN,
        INTLIT(0),
        SEMICOLON,
        ID("i"),
        LESSTHAN,
        INTLIT(5),
        SEMICOLON,
        ID("i"),
        INCREMENT,
        RPAREN,
        BREAK
      ).statement shouldBe For(
        List(VarDecl(VariableID("i"), None, Some(IntLit(0)), Set(Private()))),
        LessThan(VariableID("i"), IntLit(5)),
        List(PostIncrement(VariableID("i"))),
        Break()
      )
    }

    test("With no initialization") {
      parser(
        FOR,
        LPAREN,
        SEMICOLON,
        ID("i"),
        LESSTHAN,
        INTLIT(5),
        SEMICOLON,
        ID("i"),
        INCREMENT,
        RPAREN,
        BREAK
      ).statement shouldBe For(
        Nil,
        LessThan(VariableID("i"), IntLit(5)),
        List(PostIncrement(VariableID("i"))),
        Break()
      )
    }

    test("With no condition") {
      parser(
        FOR,
        LPAREN,
        PRIVVAR,
        ID("i"),
        EQSIGN,
        INTLIT(0),
        SEMICOLON,
        SEMICOLON,
        ID("i"),
        INCREMENT,
        RPAREN,
        BREAK
      ).statement shouldBe For(
        List(VarDecl(VariableID("i"), None, Some(IntLit(0)), Set(Private()))),
        TrueLit(),
        List(PostIncrement(VariableID("i"))),
        Break()
      )
    }

    test("With no post operation") {
      parser(
        FOR,
        LPAREN,
        PRIVVAR,
        ID("i"),
        EQSIGN,
        INTLIT(0),
        SEMICOLON,
        ID("i"),
        LESSTHAN,
        INTLIT(5),
        SEMICOLON,
        RPAREN,
        BREAK
      ).statement shouldBe For(
        List(VarDecl(VariableID("i"), None, Some(IntLit(0)), Set(Private()))),
        LessThan(VariableID("i"), IntLit(5)),
        Nil,
        Break()
      )
    }

    test("With none of the above") {
      parser(
        FOR,
        LPAREN,
        SEMICOLON,
        SEMICOLON,
        RPAREN,
        BREAK
      ).statement shouldBe For(
        Nil,
        TrueLit(),
        Nil,
        Break()
      )
    }
  }


  it should "parse for each loops" in {
    parser(
      FOR,
      LPAREN,
      PRIVVAR,
      ID("i"),
      IN,
      ID("X"),
      RPAREN,
      BREAK
    ).statement shouldBe Foreach(
      VarDecl(VariableID("i"), None, None, Set(Private())),
      VariableID("X"),
      Break()
    )
  }


  it should "parse print and error statements" in {
    parser(PRINT, LPAREN, STRLIT("ABC"), RPAREN).statement shouldBe Print(StringLit("ABC"))
    parser(PRINTLN, LPAREN, STRLIT("ABC"), RPAREN).statement shouldBe Println(StringLit("ABC"))
    parser(ERROR, LPAREN, STRLIT("ABC"), RPAREN).statement shouldBe Error(StringLit("ABC"))
    parser(PRINT, LPAREN, RPAREN).statement shouldBe Print(StringLit(""))
  }


  it should "parse return statements" in {
    test("Return a value") {
      parser(RETURN, STRLIT("ABC")).statement shouldBe Return(Some(StringLit("ABC")))
    }

    test("Return with no value") {
      parser(RETURN).statement shouldBe Return(None)
    }

    test("Return with no value if return value is on the next line") {
      parser(RETURN, NEWLINE, INTLIT(1), PLUS, INTLIT(1)).statement shouldBe Return(None)
    }
  }


  it should "parse break and continue statements" in {
    parser(CONTINUE).statement shouldBe Continue()
    parser(BREAK).statement shouldBe Break()
  }


  it should "end statements at semicolon or newline" ignore { pending }


  //------------------------------------------------------------------------------------
  //--- Expressions
  //------------------------------------------------------------------------------------


  it should "parse assignments" in {
    test("Normal assignment") {
      parser(ID("x"), EQSIGN, INTLIT(1)).expression shouldBe Assign(VariableID("x"), IntLit(1))
    }

    test("Array assignment") {
      parser(ID("x"), LBRACKET, INTLIT(1), RBRACKET, EQSIGN, INTLIT(1)).expression shouldBe
        Assign(ArrayRead(VariableID("x"), IntLit(1)), IntLit(1))
    }
  }

  it should "parse operator assignments" in {
    test("Normal assignments") {
      parser(ID("x"), PLUSEQ, INTLIT(1)).expression shouldBe Assign(VariableID("x"), Plus(VariableID("x"), IntLit(1)))
      parser(ID("x"), MINUSEQ, INTLIT(1)).expression shouldBe Assign(VariableID("x"), Minus(VariableID("x"), IntLit(1)))
      parser(ID("x"), MULEQ, INTLIT(1)).expression shouldBe Assign(VariableID("x"), Times(VariableID("x"), IntLit(1)))
      parser(ID("x"), DIVEQ, INTLIT(1)).expression shouldBe Assign(VariableID("x"), Div(VariableID("x"), IntLit(1)))
      parser(ID("x"), MODEQ, INTLIT(1)).expression shouldBe Assign(VariableID("x"), Modulo(VariableID("x"), IntLit(1)))
      parser(ID("x"), ANDEQ, INTLIT(1)).expression shouldBe Assign(VariableID("x"), LogicAnd(VariableID("x"), IntLit(1)))
      parser(ID("x"), OREQ, INTLIT(1)).expression shouldBe Assign(VariableID("x"), LogicOr(VariableID("x"), IntLit(1)))
      parser(ID("x"), XOREQ, INTLIT(1)).expression shouldBe Assign(VariableID("x"), LogicXor(VariableID("x"), IntLit(1)))
      parser(ID("x"), LSHIFTEQ, INTLIT(1)).expression shouldBe Assign(VariableID("x"), LeftShift(VariableID("x"), IntLit(1)))
      parser(ID("x"), RSHIFTEQ, INTLIT(1)).expression shouldBe Assign(VariableID("x"), RightShift(VariableID("x"), IntLit(1)))
    }

    test("Array assignments") {
      parser(ID("x"), LBRACKET, INTLIT(5), RBRACKET, PLUSEQ, INTLIT(1)).expression shouldBe
        Assign(ArrayRead(VariableID("x"), IntLit(5)), Plus(ArrayRead(VariableID("x"), IntLit(5)), IntLit(1)))
      parser(ID("x"), LBRACKET, INTLIT(5), RBRACKET, MINUSEQ, INTLIT(1)).expression shouldBe
        Assign(ArrayRead(VariableID("x"), IntLit(5)), Minus(ArrayRead(VariableID("x"), IntLit(5)), IntLit(1)))
      parser(ID("x"), LBRACKET, INTLIT(5), RBRACKET, MULEQ, INTLIT(1)).expression shouldBe
        Assign(ArrayRead(VariableID("x"), IntLit(5)), Times(ArrayRead(VariableID("x"), IntLit(5)), IntLit(1)))
      parser(ID("x"), LBRACKET, INTLIT(5), RBRACKET, DIVEQ, INTLIT(1)).expression shouldBe
        Assign(ArrayRead(VariableID("x"), IntLit(5)), Div(ArrayRead(VariableID("x"), IntLit(5)), IntLit(1)))
      parser(ID("x"), LBRACKET, INTLIT(5), RBRACKET, MODEQ, INTLIT(1)).expression shouldBe
        Assign(ArrayRead(VariableID("x"), IntLit(5)), Modulo(ArrayRead(VariableID("x"), IntLit(5)), IntLit(1)))
      parser(ID("x"), LBRACKET, INTLIT(5), RBRACKET, ANDEQ, INTLIT(1)).expression shouldBe
        Assign(ArrayRead(VariableID("x"), IntLit(5)), LogicAnd(ArrayRead(VariableID("x"), IntLit(5)), IntLit(1)))
      parser(ID("x"), LBRACKET, INTLIT(5), RBRACKET, OREQ, INTLIT(1)).expression shouldBe
        Assign(ArrayRead(VariableID("x"), IntLit(5)), LogicOr(ArrayRead(VariableID("x"), IntLit(5)), IntLit(1)))
      parser(ID("x"), LBRACKET, INTLIT(5), RBRACKET, XOREQ, INTLIT(1)).expression shouldBe
        Assign(ArrayRead(VariableID("x"), IntLit(5)), LogicXor(ArrayRead(VariableID("x"), IntLit(5)), IntLit(1)))
      parser(ID("x"), LBRACKET, INTLIT(5), RBRACKET, LSHIFTEQ, INTLIT(1)).expression shouldBe
        Assign(ArrayRead(VariableID("x"), IntLit(5)), LeftShift(ArrayRead(VariableID("x"), IntLit(5)), IntLit(1)))
      parser(ID("x"), LBRACKET, INTLIT(5), RBRACKET, RSHIFTEQ, INTLIT(1)).expression shouldBe
        Assign(ArrayRead(VariableID("x"), IntLit(5)), RightShift(ArrayRead(VariableID("x"), IntLit(5)), IntLit(1)))
    }
  }

  it should "parse ternary expressions" in {
    parser(ID("x"), QUESTIONMARK, INTLIT(1), COLON, INTLIT(2)).expression shouldBe
      Ternary(VariableID("x"), IntLit(1), IntLit(2))

    parser(ID("x"), QUESTIONMARK, INTLIT(1), COLON, ID("y"), QUESTIONMARK, INTLIT(2), COLON, INTLIT(3)).expression shouldBe
      Ternary(VariableID("x"), IntLit(1), Ternary(VariableID("y"), IntLit(2), IntLit(3)))

    parser(ID("x"), QUESTIONMARK, ID("y"), QUESTIONMARK, INTLIT(1), COLON, INTLIT(2), COLON, INTLIT(3)).expression shouldBe
      Ternary(VariableID("x"), Ternary(VariableID("y"), IntLit(1), IntLit(2)), IntLit(3))
  }

  it should "parse elvis expressions" in {
    parser(ID("x"), ELVIS, INTLIT(1)).expression shouldBe Elvis(VariableID("x"), IntLit(1))

    parser(ID("x"), ELVIS, ID("y"), ELVIS, INTLIT(1)).expression shouldBe
      Elvis(VariableID("x"), Elvis(VariableID("y"), IntLit(1)))
  }

  it should "parse or expressions" in {
    parser(ID("x"), OR, INTLIT(1)).expression shouldBe Or(VariableID("x"), IntLit(1))

    parser(ID("x"), OR, INTLIT(1), OR, ID("y"), OR, INTLIT(2)).expression shouldBe
      Or(Or(Or(VariableID("x"), IntLit(1)), VariableID("y")), IntLit(2))
  }

  it should "parse and expressions" in {
    parser(ID("x"), AND, INTLIT(1)).expression shouldBe And(VariableID("x"), IntLit(1))

    parser(ID("x"), AND, INTLIT(1), AND, ID("y"), AND, INTLIT(2)).expression shouldBe
      And(And(And(VariableID("x"), IntLit(1)), VariableID("y")), IntLit(2))
  }

  it should "parse logicOr expressions" in {
    parser(ID("x"), LOGICOR, INTLIT(1)).expression shouldBe LogicOr(VariableID("x"), IntLit(1))

    parser(ID("x"), LOGICOR, INTLIT(1), LOGICOR, ID("y"), LOGICOR, INTLIT(2)).expression shouldBe
      LogicOr(LogicOr(LogicOr(VariableID("x"), IntLit(1)), VariableID("y")), IntLit(2))
  }

  it should "parse logicXor expressions" in {
    parser(ID("x"), LOGICXOR, INTLIT(1)).expression shouldBe LogicXor(VariableID("x"), IntLit(1))

    parser(ID("x"), LOGICXOR, INTLIT(1), LOGICXOR, ID("y"), LOGICXOR, INTLIT(2)).expression shouldBe
      LogicXor(LogicXor(LogicXor(VariableID("x"), IntLit(1)), VariableID("y")), IntLit(2))
  }

  it should "parse logicAnd expressions" in {
    parser(ID("x"), LOGICAND, INTLIT(1)).expression shouldBe LogicAnd(VariableID("x"), IntLit(1))

    parser(ID("x"), LOGICAND, INTLIT(1), LOGICAND, ID("y"), LOGICAND, INTLIT(2)).expression shouldBe
      LogicAnd(LogicAnd(LogicAnd(VariableID("x"), IntLit(1)), VariableID("y")), IntLit(2))
  }

  it should "parse equals and not equals expressions" in {
    parser(ID("x"), EQUALS, INTLIT(1)).expression shouldBe Equals(VariableID("x"), IntLit(1))
    parser(ID("x"), NOTEQUALS, INTLIT(1)).expression shouldBe NotEquals(VariableID("x"), IntLit(1))

    parser(ID("x"), EQUALS, INTLIT(1), EQUALS, ID("y"), EQUALS, INTLIT(2)).expression shouldBe
      Equals(Equals(Equals(VariableID("x"), IntLit(1)), VariableID("y")), IntLit(2))
  }

  it should "parse is expressions" in {
    parser(ID("x"), IS, ID("Int")).expression shouldBe Is(VariableID("x"), ClassID("Int"))

    parser(ID("x"), IS, ID("y"), IS, ID("z"), IS, ID("w")).expression shouldBe
      Is(Is(Is(VariableID("x"), ClassID("y")), ClassID("z")), ClassID("w"))
  }

  it should "parse comparison expressions" in {
    parser(ID("x"), LESSTHAN, INTLIT(1)).expression shouldBe LessThan(VariableID("x"), IntLit(1))
    parser(ID("x"), LESSTHANEQ, INTLIT(1)).expression shouldBe LessThanEquals(VariableID("x"), IntLit(1))
    parser(ID("x"), GREATERTHAN, INTLIT(1)).expression shouldBe GreaterThan(VariableID("x"), IntLit(1))
    parser(ID("x"), GREATERTHANEQ, INTLIT(1)).expression shouldBe GreaterThanEquals(VariableID("x"), IntLit(1))

    parser(ID("x"), LESSTHAN, INTLIT(1), LESSTHAN, ID("y"), LESSTHAN, INTLIT(2)).expression shouldBe
      LessThan(LessThan(LessThan(VariableID("x"), IntLit(1)), VariableID("y")), IntLit(2))
  }

  it should "parse bitshift expressions" in {
    parser(ID("x"), LSHIFT, INTLIT(1)).expression shouldBe LeftShift(VariableID("x"), IntLit(1))
    parser(ID("x"), RSHIFT, INTLIT(1)).expression shouldBe RightShift(VariableID("x"), IntLit(1))

    parser(ID("x"), LSHIFT, INTLIT(1), LSHIFT, ID("y"), LSHIFT, INTLIT(2)).expression shouldBe
      LeftShift(LeftShift(LeftShift(VariableID("x"), IntLit(1)), VariableID("y")), IntLit(2))
  }

  it should "parse plus and minus expressions" in {
    parser(ID("x"), PLUS, INTLIT(1)).expression shouldBe Plus(VariableID("x"), IntLit(1))
    parser(ID("x"), MINUS, INTLIT(1)).expression shouldBe Minus(VariableID("x"), IntLit(1))

    parser(ID("x"), PLUS, INTLIT(1), PLUS, ID("y"), PLUS, INTLIT(2)).expression shouldBe
      Plus(Plus(Plus(VariableID("x"), IntLit(1)), VariableID("y")), IntLit(2))
  }

  it should "parse times, division and modulo expressions" in {
    parser(ID("x"), TIMES, INTLIT(1)).expression shouldBe Times(VariableID("x"), IntLit(1))
    parser(ID("x"), DIV, INTLIT(1)).expression shouldBe Div(VariableID("x"), IntLit(1))
    parser(ID("x"), MODULO, INTLIT(1)).expression shouldBe Modulo(VariableID("x"), IntLit(1))

    parser(ID("x"), TIMES, INTLIT(1), TIMES, ID("y"), TIMES, INTLIT(2)).expression shouldBe
      Times(Times(Times(VariableID("x"), IntLit(1)), VariableID("y")), IntLit(2))
  }

  it should "parse binary operators with the correct precedence" in {
    // x = true ?  a : b ?: c || d && e == f < g | h ^ i & j << k + l * m is n
    // (x = (true ?  a : (b ?: (c || (d && (e == ((f < (g | (h ^ (i & (j << (k + (l * m))))))) is n)))))))
    parser(
      ID("x"),
      EQSIGN,
      TRUE,
      QUESTIONMARK,
      ID("a"),
      COLON,
      ID("b"),
      ELVIS,
      ID("c"),
      OR,
      ID("d"),
      AND,
      ID("e"),
      EQUALS,
      ID("f"),
      LESSTHAN,
      ID("g"),
      LOGICOR,
      ID("h"),
      LOGICXOR,
      ID("i"),
      LOGICAND,
      ID("j"),
      LSHIFT,
      ID("k"),
      PLUS,
      ID("l"),
      TIMES,
      ID("m"),
      IS,
      ID("n")
    ).expression shouldBe Assign(
      VariableID("x"),
      Ternary(TrueLit(), VariableID("a"),
        Elvis(VariableID("b"),
          Or(VariableID("c"),
            And(VariableID("d"),
              Equals(VariableID("e"),
                Is(
                  LessThan(VariableID("f"),
                    LogicOr(VariableID("g"),
                      LogicXor(VariableID("h"),
                        LogicAnd(VariableID("i"),
                          LeftShift(VariableID("j"),
                            Plus(VariableID("k"),
                              Times(VariableID("l"), VariableID("m"))
                            )
                          )
                        )
                      )
                    )
                  ),
                  ClassID("n")
                )
              )
            )
          )
        )
      )
    )

    // 1 * 2 + 3
    // (1 * 2 ) + 3
    parser(INTLIT(1), TIMES, INTLIT(2), PLUS, INTLIT(3)).expression shouldBe Plus(Times(IntLit(1), IntLit(2)), IntLit(3))

    // 1 / 2 - 3 || 4 & 5 << 6
    // ((1 / 2) - 3) || (4 & (5 << 6))
    parser(
      INTLIT(1),
      DIV,
      INTLIT(2),
      MINUS,
      INTLIT(3),
      OR,
      INTLIT(4),
      LOGICAND,
      INTLIT(5),
      LSHIFT,
      INTLIT(6)
    ).expression shouldBe Or(
      Minus(Div(IntLit(1), IntLit(2)), IntLit(3)),
      LogicAnd(IntLit(4), LeftShift(IntLit(5), IntLit(6)))
    )

    // 1 % 2 is Int
    // (1 % 2) is Int
    parser(INTLIT(1), MODULO, INTLIT(2), IS, ID("Int")).expression shouldBe Is(Modulo(IntLit(1), IntLit(2)), ClassID("Int"))

    // 1 & 2 is Int
    // (1 & 2) is Int
    parser(INTLIT(1), LOGICAND, INTLIT(2), IS, ID("Int")).expression shouldBe Is(LogicAnd(IntLit(1), IntLit(2)), ClassID("Int"))

    // 1 & 2 == 0
    // (1 & 2) == 0
    parser(INTLIT(1), LOGICAND, INTLIT(2), EQUALS, INTLIT(0)).expression shouldBe Equals(LogicAnd(IntLit(1), IntLit(2)), IntLit(0))

    // 1 ^ 2 && 0
    // (1 ^ 2) && 0
    parser(INTLIT(1), LOGICXOR, INTLIT(2), AND, INTLIT(0)).expression shouldBe And(LogicXor(IntLit(1), IntLit(2)), IntLit(0))
  }

  it should "parse binary opeators with parentheses with the correct precedence" in {
    parser(INTLIT(1), TIMES, LPAREN, INTLIT(2), PLUS, INTLIT(3), RPAREN)
      .expression shouldBe Times(IntLit(1), Plus(IntLit(2), IntLit(3)))

    // (a || ((b && c) == (d < e))) | ((f ^ ((g & h) << i)) + (j * (k is l)))
    parser(
      LPAREN,
      ID("a"),
      OR,
      LPAREN,
      ID("b"),
      AND,
      ID("c"),
      RPAREN,
      EQUALS,
      LPAREN,
      ID("d"),
      LESSTHAN,
      ID("e"),
      RPAREN,
      RPAREN,
      LOGICOR,
      LPAREN,
      ID("f"),
      LOGICXOR,
      LPAREN,
      ID("g"),
      LOGICAND,
      ID("h"),
      RPAREN,
      LSHIFT,
      ID("i"),
      RPAREN,
      PLUS,
      LPAREN,
      ID("j"),
      TIMES,
      LPAREN,
      ID("k"),
      IS,
      ID("l"),
      RPAREN,
      RPAREN
    ).expression shouldBe LogicOr(
      Or(
        VariableID("a"),
        Equals(
          And(VariableID("b"), VariableID("c")),
          LessThan(VariableID("d"), VariableID("e"))
        )
      ),
      Plus(
        LogicXor(
          VariableID("f"),
          LeftShift(
            LogicAnd(VariableID("g"), VariableID("h")),
            VariableID("i")
          )
        ),
        Times(
          VariableID("j"),
          Is(
            VariableID("k"),
            ClassID("l")
          )
        )
      )
    )
  }

  //------------------------------------------------------------------------------------
  //--- Terms
  //------------------------------------------------------------------------------------

  it should "parse array literals" in {
    parser(LBRACKET, RBRACKET).expression shouldBe ArrayLit(Nil)
    parser(LBRACKET, INTLIT(1), RBRACKET).expression shouldBe ArrayLit(List(IntLit(1)))
    parser(LBRACKET, INTLIT(1), COMMA, INTLIT(2), COMMA, INTLIT(3), RBRACKET)
      .expression shouldBe ArrayLit(List(IntLit(1), IntLit(2), IntLit(3)))

    parser(LBRACKET, NEWLINE, INTLIT(1), COMMA, NEWLINE, INTLIT(2), COMMA, NEWLINE, INTLIT(3), NEWLINE, RBRACKET)
      .expression shouldBe ArrayLit(List(IntLit(1), IntLit(2), IntLit(3)))
  }

  it should "parse not expressions" in {
    parser(BANG, TRUE).expression shouldBe Not(TrueLit())
    parser(BANG, TRUE, AND, FALSE).expression shouldBe And(Not(TrueLit()), FalseLit())
    parser(BANG, LPAREN, TRUE, AND, FALSE, RPAREN).expression shouldBe Not(And(TrueLit(), FalseLit()))
  }

  it should "parse unary minus expressions" in {
    parser(MINUS, TRUE).expression shouldBe Negation(TrueLit())
    parser(MINUS, INTLIT(1)).expression shouldBe IntLit(-1)
    parser(MINUS, LONGLIT(1)).expression shouldBe LongLit(-1)
    parser(MINUS, FLOATLIT(1)).expression shouldBe FloatLit(-1)
    parser(MINUS, DOUBLELIT(1)).expression shouldBe DoubleLit(-1)
    parser(MINUS, CHARLIT('a')).expression shouldBe IntLit(-97)
  }

  it should "parse hash expressions" in {
    parser(HASH, TRUE).expression shouldBe Hash(TrueLit())
  }

  it should "parse increment/decrement expressions" in {
    parser(INCREMENT, ID("a")).expression shouldBe PreIncrement(VariableID("a"))
    parser(DECREMENT, ID("a")).expression shouldBe PreDecrement(VariableID("a"))

    parser(ID("a"), INCREMENT).expression shouldBe PostIncrement(VariableID("a"))
    parser(ID("a"), DECREMENT).expression shouldBe PostDecrement(VariableID("a"))

    // ++--++a++++--
    parser(INCREMENT, DECREMENT, INCREMENT, ID("a"), INCREMENT, INCREMENT, DECREMENT).expression shouldBe
      PreIncrement(PreDecrement(PreIncrement(PostDecrement(PostIncrement(PostIncrement(VariableID("a")))))))
  }

  it should "parse literals" in {
    parser(INTLIT(1)).expression shouldBe IntLit(1)
    parser(LONGLIT(1)).expression shouldBe LongLit(1)
    parser(FLOATLIT(1)).expression shouldBe FloatLit(1)
    parser(DOUBLELIT(1)).expression shouldBe DoubleLit(1)
    parser(CHARLIT('a')).expression shouldBe CharLit('a')
    parser(STRLIT("ABC")).expression shouldBe StringLit("ABC")
  }


  it should "parse identifiers" in {
    parser(ID("x")).expression shouldBe VariableID("x")
  }

  it should "parse true, false, this and null expressions" in {
    parser(TRUE).expression shouldBe TrueLit()
    parser(FALSE).expression shouldBe FalseLit()
    parser(THIS).expression shouldBe This()
    parser(NULL).expression shouldBe NullLit()
  }

  it should "parse super expressions" in {
    parser(SUPER, DOT, ID("a")).expression shouldBe NormalAccess(Super(None), VariableID("a"))

    parser(SUPER, LESSTHAN, ID("A"), GREATERTHAN, DOT, ID("a"), LPAREN, INTLIT(1), RPAREN)
      .expression shouldBe NormalAccess(Super(Some(ClassID("A"))), MethodCall(MethodID("a"), List(IntLit(1))))
  }

  it should "parse new expressions" in {
    // new A()
    parser(NEW, ID("A"), LPAREN, RPAREN).expression shouldBe New(ClassID("A"), Nil)

    // new A(1)
    parser(NEW, ID("A"), LPAREN, INTLIT(1), RPAREN).expression shouldBe New(ClassID("A"), List(IntLit(1)))

    // new A(
    //  1,
    //  2,
    //  3
    // )
    parser(NEW, ID("A"), LPAREN, NEWLINE, INTLIT(1), COMMA, NEWLINE, INTLIT(2), COMMA, NEWLINE, INTLIT(3), NEWLINE, RPAREN)
      .expression shouldBe New(ClassID("A"), List(IntLit(1), IntLit(2), IntLit(3)))

    // new A<T>()
    parser(NEW, ID("A"), LESSTHAN, ID("T"), GREATERTHAN, LPAREN, RPAREN)
      .expression shouldBe New(ClassID("A", List(ClassID("T"))), Nil)

    // new A[5]
    parser(NEW, ID("A"), LBRACKET, INTLIT(5), RBRACKET)
      .expression shouldBe NewArray(ArrayType(ClassID("A")), List(IntLit(5)))

    // new A<T>[5]
    parser(NEW, ID("A"), LESSTHAN, ID("T"), GREATERTHAN, LBRACKET, INTLIT(5), RBRACKET)
      .expression shouldBe NewArray(ArrayType(ClassID("A", List(ClassID("T")))), List(IntLit(5)))

    // new A[1][2][3]
    parser(NEW, ID("A"), LBRACKET, INTLIT(1), RBRACKET, LBRACKET, INTLIT(2), RBRACKET, LBRACKET, INTLIT(3), RBRACKET)
      .expression shouldBe NewArray(ArrayType(ArrayType(ArrayType(ClassID("A")))), List(IntLit(1), IntLit(2), IntLit(3)))

    // new A?[5]
    parser(NEW, ID("A"), QUESTIONMARK, LBRACKET, INTLIT(5), RBRACKET)
      .expression shouldBe NewArray(ArrayType(NullableType(ClassID("A"))), List(IntLit(5)))

    // new A?[1]?[2]?[3]
    parser(
      NEW,
      ID("A"),
      QUESTIONMARK,
      LBRACKET,
      INTLIT(1),
      RBRACKET,
      QUESTIONMARK,
      LBRACKET,
      INTLIT(2),
      RBRACKET,
      QUESTIONMARK,
      LBRACKET,
      INTLIT(3),
      RBRACKET
    ).expression shouldBe NewArray(
      ArrayType(NullableType(
        ArrayType(NullableType(
          ArrayType(NullableType(
            ClassID("A"))
          ))
        ))
      ),
      List(IntLit(1), IntLit(2), IntLit(3))
    )
  }

  it should "parse access expressions" in {

  }

  it should "parse indexing expressions" ignore { pending }
  it should "parse slicing expressions" ignore { pending }
  it should "parse as expressions" ignore { pending }
  it should "parse extract nullable expressions" ignore { pending }
  it should "parse expressions with multiple " ignore { pending }


  //------------------------------------------------------------------------------------
  //--- Misc
  //------------------------------------------------------------------------------------

  it should "parse types" ignore { pending }
  it should "parse a formal" ignore { pending }
  it should "replace the last expression with a return statement" ignore { pending }
  it should "parse class type identifiers with recursive template types" ignore { pending }

  private def parser(tokens: Any*) = {
    val t = tokens.map {
      case t: Token        => t
      case kind: TokenKind => new Token(kind)
      case _               => ???
    } :+ new Token(EOF)

    val tokenStream = TokenStream(t)
    val ctx = Context(mock[Reporter], createMockFormatter(), mock[DebugOutputFormatter])
    Parser(ctx, mock[ErrorStringContext], tokenStream)
  }

}
