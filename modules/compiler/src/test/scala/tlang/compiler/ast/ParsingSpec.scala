package tlang
package compiler
package ast

import tlang.compiler.ast.Trees._
import tlang.compiler.imports.Imports
import tlang.compiler.lexer.Tokens._
import tlang.compiler.lexer.{Token, TokenKind, Tokens}
import tlang.compiler.messages.{CompilationException, Reporter}
import tlang.compiler.output.PrettyOutputHandler
import tlang.formatting.{ErrorStringContext, Formatter}
import tlang.testutils.{TreeTesting, UnitSpec}
import tlang.utils.StringSource

class ParsingSpec extends UnitSpec with TreeTesting {

  //------------------------------------------------------------------------------------
  //--- Declarations
  //------------------------------------------------------------------------------------

  // package A
  // import B
  // import C::*
  // println(1)
  // println(2)
  // Def D() = 1
  // def E(a: A) = 1
  // @AnnotationA("ABC", 1) class F
  // trait G
  it should "parse a compilation unit" in {
    parser(
      PACKAGE, ID("A"), NEWLINE,
      IMPORT, ID("B"), NEWLINE,
      IMPORT, ID("C"), COLON, COLON, TIMES, NEWLINE,
      PRINTLN, LPAREN, INTLIT(1), RPAREN, NEWLINE,
      PRINTLN, LPAREN, INTLIT(2), RPAREN, NEWLINE,
      PUBDEF, ID("D"), LPAREN, RPAREN, EQSIGN, INTLIT(1), NEWLINE,
      PRIVDEF, ID("E"), LPAREN, ID("a"), COLON, ID("A"), RPAREN, EQSIGN, INTLIT(1), NEWLINE,
      AT, ID("AnnotationA"), LPAREN, ID("a"), EQSIGN, STRLIT("ABC"), COMMA, ID("b"), EQSIGN, INTLIT(1), RPAREN,
      CLASS, ID("F"), NEWLINE,
      TRAIT, ID("G"), NEWLINE
    ).compilationUnit shouldBe CompilationUnit(
      Package(List("A")),
      classes = List(
        ClassDecl("ParsingSpec",
          parents = Nil,
          fields = Nil,
          methods = List(
            MethodDecl("main",
              modifiers = Set(Public(), Static()),
              args = List(Formal(ArrayType("java::lang::String"), "args")),
              retType = UnitType(),
              stat = Block(List(
                Println(IntLit(1)),
                Println(IntLit(2))
              ))
            ),
            MethodDecl("D",
              modifiers = Set(Public(), Static()),
              args = Nil,
              retType = None,
              stat = Return(IntLit(1))
            ),
            MethodDecl("E",
              modifiers = Set(Private(), Static()),
              args = List(Formal("A", "a")),
              retType = None,
              stat = Return(IntLit(1))
            )
          )
        ),
        ClassDecl("F",
          annotations = List(Annotation("AnnotationA", List(
            KeyValuePair("a", StringLit("ABC")),
            KeyValuePair("b", IntLit(1))
          )))
        ),
        TraitDecl("G")
      ),
      imports = Imports(
        ctx,
        errorStringContext,
        imports = List(RegularImport(List("B")), WildCardImport(List("C")))
      )
    )
  }

  it should "parse a package declaration" in {
    // package A
    parser(PACKAGE, ID("A")).packageDeclaration shouldBe Package(List("A"))

    // package A::B::C
    parser(PACKAGE, ID("A"), COLON, COLON, ID("B"), COLON, COLON, ID("C"))
      .packageDeclaration shouldBe Package(List("A", "B", "C"))

    // package A::B::
    // C
    parser(PACKAGE, ID("A"), COLON, COLON, ID("B"), COLON, COLON, NEWLINE, ID("C"))
      .packageDeclaration shouldBe Package(List("A", "B", "C"))
  }

  it should "parse a import declaration" in {
    // import A
    parser(IMPORT, ID("A")).importDeclaration shouldBe RegularImport(List("A"))

    // import A::B::C
    parser(IMPORT, ID("A"), COLON, COLON, ID("B"), COLON, COLON, ID("C"))
      .importDeclaration shouldBe RegularImport(List("A", "B", "C"))

    // import package A::B::
    // C
    parser(IMPORT, ID("A"), COLON, COLON, ID("B"), COLON, COLON, NEWLINE, ID("C"))
      .importDeclaration shouldBe RegularImport(List("A", "B", "C"))

    // import A::B::*
    parser(IMPORT, ID("A"), COLON, COLON, ID("B"), COLON, COLON, TIMES)
      .importDeclaration shouldBe WildCardImport(List("A", "B"))
  }

  it should "parse an annotation" in {
    // @A
    parser(AT, ID("A")).annotation shouldBe Annotation("A", Nil)

    // @A()
    parser(AT, ID("A"), LPAREN, RPAREN).annotation shouldBe Annotation("A", Nil)

    // @A(a = 1, b = "ABC", c = 3)
    parser(AT, ID("A"), LPAREN, ID("a"), EQSIGN, INTLIT(1), COMMA, ID("b"), EQSIGN, STRLIT("ABC"),
      COMMA, ID("c"), EQSIGN, INTLIT(3), RPAREN)
      .annotation shouldBe Annotation("A", List(
      KeyValuePair(VariableID("a"), IntLit(1)),
      KeyValuePair(VariableID("b"), StringLit("ABC")),
      KeyValuePair(VariableID("c"), IntLit(3))
    ))

    // @A(
    //    a = 1,
    //    b = "ABC",
    //    c = 3,
    // )
    parser(AT, ID("A"), LPAREN, NEWLINE,
      ID("a"), EQSIGN, INTLIT(1), COMMA, NEWLINE,
      ID("b"), EQSIGN, STRLIT("ABC"), COMMA, NEWLINE,
      ID("c"), EQSIGN, INTLIT(3), COMMA,
      RPAREN)
      .annotation shouldBe Annotation("A", List(
      KeyValuePair(VariableID("a"), IntLit(1)),
      KeyValuePair(VariableID("b"), StringLit("ABC")),
      KeyValuePair(VariableID("c"), IntLit(3))
    ))

    // @A::B::C
    parser(AT, ID("A"), COLON, COLON, ID("B"), COLON, COLON, ID("C"))
      .annotation shouldBe Annotation("A::B::C", Nil)

    // @A<B>
    parser(AT, ID("A"), LESSTHAN, ID("B"), GREATERTHAN)
      .annotation shouldBe Annotation(ClassID("A", List("B")), Nil)
  }

  it should "parse a class declaration" in {
    // class A
    parser(CLASS, ID("A"))
      .classDeclaration(Nil) shouldBe ClassDecl("A")

    // class A : B
    parser(CLASS, ID("A"), COLON, ID("B")).classDeclaration(Nil) shouldBe ClassDecl("A", List("B"))

    // class A : B, C, D
    parser(CLASS, ID("A"), COLON, ID("B"), COMMA, ID("C"), COMMA, ID("D"))
      .classDeclaration(Nil) shouldBe ClassDecl("A", List("B", "C", "D"))

    // class A : B,
    //           C,
    //           D =
    //    var a
    parser(
      CLASS, ID("A"), COLON, ID("B"), COMMA, NEWLINE, ID("C"), COMMA, NEWLINE, ID("D"), EQSIGN, NEWLINE, INDENT, PRIVVAR,
      ID("a"), NEWLINE, DEDENT
    ).classDeclaration(Nil) shouldBe ClassDecl("A",
      parents = List("B", "C", "D"),
      fields = List(VarDecl("a", modifiers = Set(Private())))
    )

    // class A<B> : C, D =
    //  var a
    //  var b
    //  def x()
    //  def y()
    parser(
      CLASS, ID("A"), LESSTHAN, ID("B"), GREATERTHAN, COLON, ID("C"), COMMA, ID("D"), EQSIGN, NEWLINE, INDENT, PRIVVAR,
      ID("a"), NEWLINE, PRIVVAR, ID("b"), NEWLINE, PRIVDEF, ID("x"), LPAREN, RPAREN, NEWLINE, PRIVDEF, ID("y"), LPAREN,
      RPAREN, NEWLINE, DEDENT
    ).classDeclaration(Nil) shouldBe ClassDecl(
      ClassID("A", List("B")),
      parents = List("C", "D"),
      fields = List(
        VarDecl("a", modifiers = Set(Private())),
        VarDecl("b", modifiers = Set(Private()))
      ),
      methods = List(
        MethodDecl("x", modifiers = Set(Private())),
        MethodDecl("y", modifiers = Set(Private()))
      )
    )
  }

  it should "parse a trait declaration" in {
    // trait A<B>
    parser(TRAIT, ID("A"), LESSTHAN, ID("B"), GREATERTHAN)
      .traitDeclaration(Nil) shouldBe TraitDecl(ClassID("A", List("B")))

    // trait A<B> : C
    parser(TRAIT, ID("A"), LESSTHAN, ID("B"), GREATERTHAN, COLON, ID("C"))
      .traitDeclaration(Nil) shouldBe TraitDecl(ClassID("A", List("B")), List("C"))

    // trait A =
    //  var a
    //  var b
    parser(TRAIT, ID("A"), EQSIGN, NEWLINE, INDENT, PRIVVAR, ID("a"), NEWLINE, PRIVVAR, ID("b"), NEWLINE, DEDENT)
      .traitDeclaration(Nil) shouldBe TraitDecl(
      ClassID("A"),
      parents = Nil,
      fields = List(
        VarDecl("a", modifiers = Set(Private())),
        VarDecl("b", modifiers = Set(Private()))
      )
    )
  }

  it should "parse an extension declaration" in {
    // extension A: B
    parser(EXTENSION, ID("A"), COLON, ID("B")).extensionDeclaration(Nil) shouldBe ExtensionDecl("A", "B")

    // extension A<T>: B<C<T, Int>>
    parser(EXTENSION, ID("A"), LESSTHAN, ID("T"), GREATERTHAN, COLON, ID("B"), LESSTHAN, ID("C"), LESSTHAN,
      ID("T"), COMMA, ID("Int"), RSHIFT)
      .extensionDeclaration(Nil) shouldBe ExtensionDecl(
      ClassID("A", "T" :: Nil),
      ClassID("B", ClassID("C", ClassID("T", Nil) :: ClassID("Int", Nil) :: Nil) :: Nil)
    )

    // extension A: B =
    //  def x()
    //  def y()
    parser(
      EXTENSION, ID("A"), COLON, ID("B"), EQSIGN, NEWLINE, INDENT,
      PRIVDEF, ID("x"), LPAREN, RPAREN, NEWLINE,
      PRIVDEF, ID("y"), LPAREN, RPAREN, NEWLINE, DEDENT
    ).extensionDeclaration(Nil) shouldBe ExtensionDecl("A", "B",
      methods = List(
        MethodDecl("x", modifiers = Set(Private())),
        MethodDecl("y", modifiers = Set(Private()))
      )
    )

    // No fields
    a[ParsingException] should be thrownBy parser(
      EXTENSION, ID("A"), EQSIGN, NEWLINE, INDENT, PRIVVAR, ID("x"), NEWLINE, DEDENT
    ).extensionDeclaration(Nil)
  }

  it should "parse an annotation declaration" in {
    // annotation A
    parser(ANNOTATION, ID("A")).annotationDeclaration(Nil) shouldBe AnnotationDecl(ClassID("A"))

    // annotation A =
    //  Def x()
    //  Def y()
    parser(
      ANNOTATION, ID("A"), EQSIGN, NEWLINE, INDENT,
      PUBDEF, ID("x"), LPAREN, RPAREN, NEWLINE,
      PUBDEF, ID("y"), LPAREN, RPAREN, NEWLINE,
      DEDENT
    ).annotationDeclaration(Nil) shouldBe AnnotationDecl("A",
      methods = List(
        MethodDecl("x", modifiers = Set(Public())),
        MethodDecl("y", modifiers = Set(Public()))
      )
    )
  }

  it should "parse a field declaration" in {
    // var x
    parser(PRIVVAR, ID("x"))
      .fieldDeclaration(Nil) shouldBe VarDecl("x",
      tpe = None,
      initiation = None,
      modifiers = Set(Private())
    )

    // var x: Int
    parser(PRIVVAR, ID("x"), COLON, ID("Int"))
      .fieldDeclaration(Nil) shouldBe VarDecl("x",
      tpe = Some("Int"),
      initiation = None,
      modifiers = Set(Private())
    )

    // Var static x = 1
    parser(PUBVAR, STATIC, ID("x"), EQSIGN, INTLIT(1))
      .fieldDeclaration(Nil) shouldBe VarDecl("x",
      tpe = None,
      initiation = IntLit(1),
      modifiers = Set(Public(), Static())
    )

    // Var static x: Int = 1
    parser(PUBVAL, STATIC, ID("x"), COLON, ID("Int"), EQSIGN, INTLIT(1))
      .fieldDeclaration(Nil) shouldBe VarDecl("x",
      tpe = Some("Int"),
      initiation = IntLit(1),
      modifiers = Set(Public(), Static(), Final())
    )

    // var protected x
    parser(PRIVVAL, PROTECTED, ID("x"))
      .fieldDeclaration(Nil) shouldBe VarDecl("x",
      tpe = None,
      initiation = None,
      modifiers = Set(Protected(), Final())
    )
  }

  it should "parse a method declaration" in {
    // def x()
    parser(PRIVDEF, ID("x"), LPAREN, RPAREN)
      .methodDeclaration(Nil) shouldBe MethodDecl("x", modifiers = Set(Private()), retType = None, args = Nil, stat = None)

    // Def x(): A
    parser(PUBDEF, ID("x"), LPAREN, RPAREN, COLON, ID("A"))
      .methodDeclaration(Nil) shouldBe MethodDecl("x", modifiers = Set(Public()), retType = Some("A"), args = Nil, stat = None
    )

    // def static x(): A = 1
    parser(PRIVDEF, STATIC, ID("x"), LPAREN, RPAREN, COLON, ID("A"), EQSIGN, INTLIT(1))
      .methodDeclaration(Nil) shouldBe MethodDecl("x",
      modifiers = Set(Private(), Static()),
      retType = Some("A"),
      args = Nil,
      stat = Return(IntLit(1))
    )

    // def static x(): A =
    //    1
    //    2
    //    3
    parser(
      PRIVDEF, STATIC, ID("x"), LPAREN, RPAREN, COLON, ID("A"), EQSIGN, NEWLINE, INDENT, INTLIT(1), NEWLINE, INTLIT(2),
      NEWLINE, INTLIT(3), NEWLINE, DEDENT
    ).methodDeclaration(Nil) shouldBe MethodDecl("x",
      modifiers = Set(Private(), Static()),
      retType = Some("A"),
      args = Nil,
      stat = Block(List(
        IntLit(1),
        IntLit(2),
        Return(IntLit(3))
      ))
    )

    // Def static x(a: A, b: B) = 1
    parser(PUBDEF, STATIC, ID("x"), LPAREN, ID("a"), COLON, ID("A"), RPAREN, EQSIGN, INTLIT(1))
      .methodDeclaration(Nil) shouldBe MethodDecl("x",
      modifiers = Set(Public(), Static()),
      retType = None,
      args = List(Formal("A", "a")),
      stat = Return(IntLit(1))
    )

    // Def x(a: A, b: B)
    parser(PUBDEF, ID("x"), LPAREN, ID("a"), COLON, ID("A"), COMMA, ID("b"), COLON, ID("B"), RPAREN)
      .methodDeclaration(Nil) shouldBe MethodDecl("x",
      modifiers = Set(Public()),
      retType = None,
      args = List(Formal("A", "a"), Formal("B", "b")),
      stat = None
    )

    // Def x(
    //    a: A,
    //    b: B,
    //    c: C,
    // )
    parser(
      PUBDEF, ID("x"), LPAREN, NEWLINE,
      ID("a"), COLON, ID("A"), COMMA, NEWLINE,
      ID("b"), COLON, ID("B"), COMMA, NEWLINE,
      ID("c"), COLON, ID("C"), COMMA, NEWLINE, RPAREN
    ).methodDeclaration(Nil) shouldBe MethodDecl("x",
      modifiers = Set(Public()),
      retType = None,
      args = List(
        Formal("A", "a"),
        Formal("B", "b"),
        Formal("C", "c")
      ),
      stat = None
    )
  }

  it should "parse a constructor declaration" in {
    // Def new()
    parser(PUBDEF, NEW, LPAREN, RPAREN).methodDeclaration(Nil) shouldBe ConstructorDecl(
      MethodID("new"),
      modifiers = Set(Public()),
      args = Nil,
      retType = UnitType(),
      stat = None
    )

    // def new(a: A, b: B)
    parser(PRIVDEF, NEW, LPAREN, ID("a"), COLON, ID("A"), COMMA, NEWLINE, ID("b"), COLON, ID("B"), RPAREN)
      .methodDeclaration(Nil) shouldBe ConstructorDecl(
      "new",
      modifiers = Set(Private()),
      args = List(Formal("A", "a"), Formal("B", "b")),
      retType = UnitType(),
      stat = None
    )

    // Def new() =
    //    1
    //    2
    //    3
    parser(
      PUBDEF, NEW, LPAREN, RPAREN, EQSIGN, NEWLINE, INDENT, INTLIT(1), NEWLINE, INTLIT(2),
      NEWLINE, INTLIT(3), NEWLINE, DEDENT
    ).methodDeclaration(Nil) shouldBe ConstructorDecl(
      "new",
      modifiers = Set(Public()),
      retType = UnitType(),
      args = Nil,
      stat = Block(List(
        IntLit(1),
        IntLit(2),
        IntLit(3)
      ))
    )

    // Def new(
    //    a: A,
    //    b: B,
    //    c: C,
    // )
    parser(
      PUBDEF, NEW, LPAREN, ID("a"), COLON, ID("A"), COMMA, NEWLINE, ID("b"), COLON, ID("B"), COMMA, NEWLINE, ID("c"),
      COLON, ID("C"), COMMA, NEWLINE, RPAREN
    ).methodDeclaration(Nil) shouldBe ConstructorDecl(
      "new",
      modifiers = Set(Public()),
      retType = UnitType(),
      args = List(
        Formal("A", "a"),
        Formal("B", "b"),
        Formal("C", "c")
      ),
      stat = None
    )
  }

  it should "parse operator declarations" in {
    // Def <tokenKind>(a: A, b: B)
    def binaryOperator(tokenKind: TokenKind, operatorType: (ExprTree, ExprTree) => OperatorTree) = {
      parser(PUBDEF, tokenKind, LPAREN, ID("a"), COLON, ID("A"), COMMA, ID("b"), COLON, ID("B"), RPAREN)
        .methodDeclaration(Nil) shouldBe OperatorDecl(
        operatorType(Empty(), Empty()),
        modifiers = Set(Public(), Static()),
        args = List(Formal("A", "a"), Formal("B", "b")),
        stat = None
      )
    }

    binaryOperator(MINUS, Minus)
    binaryOperator(PLUS, Plus)
    binaryOperator(TIMES, Times)
    binaryOperator(DIV, Div)
    binaryOperator(MODULO, Modulo)
    binaryOperator(LOGICAND, LogicAnd)
    binaryOperator(LOGICOR, LogicOr)
    binaryOperator(LOGICXOR, LogicXor)
    binaryOperator(LSHIFT, LeftShift)
    binaryOperator(RSHIFT, RightShift)
    binaryOperator(LESSTHAN, LessThan)
    binaryOperator(LESSTHANEQ, LessThanEquals)
    binaryOperator(GREATERTHAN, GreaterThan)
    binaryOperator(GREATERTHANEQ, GreaterThanEquals)
    binaryOperator(EQUALS, Equals)
    binaryOperator(NOTEQUALS, NotEquals)

    // Def <tokenKind>(a: A)
    def unaryOperator(tokenKind: TokenKind, operatorType: (ExprTree) => OperatorTree) = {
      parser(PUBDEF, tokenKind, LPAREN, ID("a"), COLON, ID("A"), RPAREN)
        .methodDeclaration(Nil) shouldBe OperatorDecl(
        operatorType(Empty()),
        modifiers = Set(Public(), Static()),
        args = List(Formal("A", "a")),
        stat = None
      )
    }

    unaryOperator(MINUS, Negation)
    unaryOperator(LOGICNOT, LogicNot)
    unaryOperator(HASH, Hash)
    unaryOperator(INCREMENT, PreIncrement)
    unaryOperator(DECREMENT, PreDecrement)

    // Def [](a: A)
    parser(PUBDEF, LBRACKET, RBRACKET, LPAREN, ID("a"), COLON, ID("A"), RPAREN)
      .methodDeclaration(Nil) shouldBe OperatorDecl(
      ArrayRead(Empty(), Empty()),
      modifiers = Set(Public()),
      args = List(Formal("A", "a")),
      stat = None
    )

    // Def []=(a: A, b: B)
    parser(PUBDEF, LBRACKET, RBRACKET, EQSIGN, LPAREN, ID("a"), COLON, ID("A"), COMMA, ID("b"), COLON, ID("B"), RPAREN)
      .methodDeclaration(Nil) shouldBe OperatorDecl(
      Assign(ArrayRead(Empty(), Empty()), Empty()),
      modifiers = Set(Public()),
      args = List(Formal("A", "a"), Formal("B", "b")),
      stat = None
    )

    // Def [:](a: A, b: B, c: C)
    parser(
      PUBDEF, LBRACKET, COLON, RBRACKET, LPAREN, ID("a"), COLON, ID("A"), COMMA, ID("b"), COLON, ID("B"),
      COMMA, ID("c"), COLON, ID("C"), RPAREN
    ).methodDeclaration(Nil) shouldBe OperatorDecl(
      ArraySlice(Empty(), None, None, None),
      modifiers = Set(Public()),
      args = List(
        Formal("A", "a"),
        Formal("B", "b"),
        Formal("C", "c")
      ),
      stat = None
    )

    // Def +(
    //    a: A,
    //    b: B,
    // ): A =
    parser(
      PUBDEF, PLUS, LPAREN, NEWLINE, ID("a"), COLON, ID("A"), COMMA, NEWLINE, ID("b"), COLON, ID("B"), COMMA, NEWLINE,
      RPAREN, COLON, ID("A"), EQSIGN, INTLIT(1)
    ).methodDeclaration(Nil) shouldBe OperatorDecl(
      Plus(Empty(), Empty()),
      modifiers = Set(Public(), Static()),
      args = List(Formal("A", "a"), Formal("B", "b")),
      retType = Some("A"),
      stat = Return(IntLit(1))
    )
  }

  it should "create a class from free statements and methods" in {
    parser(PRINTLN, LPAREN, INTLIT(1), RPAREN, NEWLINE, PRINTLN, LPAREN, INTLIT(2), RPAREN, NEWLINE)
      .compilationUnit.classes shouldBe List(
      ClassDecl("ParsingSpec",
        parents = Nil,
        fields = Nil,
        methods = List(
          MethodDecl("main",
            modifiers = Set(Public(), Static()),
            args = List(Formal(ArrayType("java::lang::String"), "args")),
            retType = UnitType(),
            stat = Block(List(
              Println(IntLit(1)),
              Println(IntLit(2))
            ))
          )
        )
      )
    )

    parser(PUBDEF, ID("X"), LPAREN, RPAREN, EQSIGN, INTLIT(1))
      .compilationUnit.classes shouldBe List(
      ClassDecl("ParsingSpec",
        parents = Nil,
        fields = Nil,
        methods = List(
          MethodDecl("X",
            modifiers = Set(Public(), Static()),
            args = Nil,
            retType = None,
            stat = Return(IntLit(1))
          )
        )
      )
    )

    parser(
      PRINTLN, LPAREN, INTLIT(1), RPAREN, NEWLINE, PRINTLN, LPAREN, INTLIT(2), RPAREN, NEWLINE, PUBDEF, ID("X"),
      LPAREN, RPAREN, EQSIGN, INTLIT(1)
    ).compilationUnit.classes shouldBe List(
      ClassDecl(
        "ParsingSpec",
        parents = Nil,
        fields = Nil,
        methods = List(
          MethodDecl(
            "main",
            modifiers = Set(Public(), Static()),
            args = List(Formal(ArrayType("java::lang::String"), "args")),
            retType = UnitType(),
            stat = Block(List(
              Println(IntLit(1)),
              Println(IntLit(2))
            ))
          ),
          MethodDecl(
            "X",
            modifiers = Set(Public(), Static()),
            args = Nil,
            retType = None,
            stat = Return(IntLit(1))
          )
        )
      )
    )
  }

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
        If(FalseLit(), Continue(), Break())
    }

    test("If else on two lines") {
      parser(IF, LPAREN, FALSE, RPAREN, CONTINUE, NEWLINE, ELSE, BREAK).statement shouldBe
        If(FalseLit(), Continue(), Break())
    }

    test("If else on one line") {
      parser(IF, LPAREN, FALSE, RPAREN, CONTINUE, ELSE, BREAK).statement shouldBe
        If(FalseLit(), Continue(), Break())
    }
  }

  it should "parse while loops" in {
    parser(WHILE, LPAREN, FALSE, RPAREN, BREAK).statement shouldBe While(FalseLit(), Break())
  }

  it should "parse for loops" in {
    test("With everything") {
      parser(
        FOR, LPAREN, PRIVVAR, ID("i"), EQSIGN, INTLIT(0), SEMICOLON, ID("i"), LESSTHAN, INTLIT(5), SEMICOLON,
        ID("i"), INCREMENT, RPAREN, BREAK
      ).statement shouldBe For(
        List(VarDecl("i", None, IntLit(0), Set(Private()))),
        LessThan(VariableID("i"), IntLit(5)),
        List(PostIncrement(VariableID("i"))),
        Break()
      )
    }

    test("With no initialization") {
      parser(FOR, LPAREN, SEMICOLON, ID("i"), LESSTHAN, INTLIT(5), SEMICOLON, ID("i"), INCREMENT, RPAREN, BREAK)
        .statement shouldBe For(
        Nil,
        LessThan(VariableID("i"), IntLit(5)),
        List(PostIncrement(VariableID("i"))),
        Break()
      )
    }

    test("With no condition") {
      parser(FOR, LPAREN, PRIVVAR, ID("i"), EQSIGN, INTLIT(0), SEMICOLON, SEMICOLON, ID("i"), INCREMENT, RPAREN, BREAK)
        .statement shouldBe For(
        List(VarDecl("i", None, IntLit(0), Set(Private()))),
        TrueLit(),
        List(PostIncrement(VariableID("i"))),
        Break()
      )
    }

    test("With no post operation") {
      parser(
        FOR, LPAREN, PRIVVAR, ID("i"), EQSIGN, INTLIT(0), SEMICOLON, ID("i"), LESSTHAN, INTLIT(5), SEMICOLON, RPAREN,
        BREAK
      ).statement shouldBe For(
        List(VarDecl("i", None, IntLit(0), Set(Private()))),
        LessThan(VariableID("i"), IntLit(5)),
        Nil,
        Break()
      )
    }

    test("With none of the above") {
      parser(FOR, LPAREN, SEMICOLON, SEMICOLON, RPAREN, BREAK).statement shouldBe For(Nil, TrueLit(), Nil, Break())
    }
  }

  it should "parse for each loops" in {
    parser(FOR, LPAREN, PRIVVAR, ID("i"), IN, ID("X"), RPAREN, BREAK).statement shouldBe Foreach(
      VarDecl("i", None, None, Set(Private())),
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
      parser(RETURN, STRLIT("ABC")).statement shouldBe Return(StringLit("ABC"))
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
      ID("x"), EQSIGN, TRUE, QUESTIONMARK, ID("a"), COLON, ID("b"), ELVIS, ID("c"), OR, ID("d"), AND, ID("e"),
      EQUALS, ID("f"), LESSTHAN, ID("g"), LOGICOR, ID("h"), LOGICXOR, ID("i"), LOGICAND, ID("j"), LSHIFT, ID("k"),
      PLUS, ID("l"), TIMES, ID("m"), IS, ID("n")
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
                )))))))

    // 1 * 2 + 3
    // (1 * 2 ) + 3
    parser(INTLIT(1), TIMES, INTLIT(2), PLUS, INTLIT(3)).expression shouldBe Plus(Times(IntLit(1), IntLit(2)), IntLit(3))

    // 1 / 2 - 3 || 4 & 5 << 6
    // ((1 / 2) - 3) || (4 & (5 << 6))
    parser(INTLIT(1), DIV, INTLIT(2), MINUS, INTLIT(3), OR, INTLIT(4), LOGICAND, INTLIT(5), LSHIFT, INTLIT(6))
      .expression shouldBe Or(
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

    // 1 == 2 as Double
    parser(INTLIT(1), EQUALS, INTLIT(2), AS, ID("Double")).expression shouldBe Equals(IntLit(1), As(IntLit(2), ClassID("Double")))

    // 1 * 2 as Double
    parser(INTLIT(1), TIMES, INTLIT(2), AS, ID("Double")).expression shouldBe As(Times(IntLit(1), IntLit(2)), ClassID("Double"))

    // 1 as Double is Double
    parser(INTLIT(1), AS, ID("Double"), IS, ID("Double")).expression shouldBe Is(As(IntLit(1), ClassID("Double")), ClassID("Double"))
  }

  it should "parse binary opeators with parentheses with the correct precedence" in {
    parser(INTLIT(1), TIMES, LPAREN, INTLIT(2), PLUS, INTLIT(3), RPAREN)
      .expression shouldBe Times(IntLit(1), Plus(IntLit(2), IntLit(3)))

    // (a || ((b && c) == (d < e))) | ((f ^ ((g & h) << i)) + (j * (k is l)))
    parser(
      LPAREN, ID("a"), OR, LPAREN, ID("b"), AND, ID("c"), RPAREN, EQUALS, LPAREN, ID("d"), LESSTHAN, ID("e"), RPAREN,
      RPAREN, LOGICOR, LPAREN, ID("f"), LOGICXOR, LPAREN, ID("g"), LOGICAND, ID("h"), RPAREN, LSHIFT, ID("i"), RPAREN,
      PLUS, LPAREN, ID("j"), TIMES, LPAREN, ID("k"), IS, ID("l"), RPAREN, RPAREN
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

  it should "parse binary operators with newlines" in {
    // 1 -
    // 2
    parser(INTLIT(1), MINUS, NEWLINE, INTLIT(2)).expression shouldBe Minus(IntLit(1), IntLit(2))

    // 1 as
    // Double
    parser(INTLIT(1), AS, NEWLINE, ID("Double")).expression shouldBe As(IntLit(1), ClassID("Double"))

    // 1
    // - 2
    val p = parser(INTLIT(1), NEWLINE, MINUS, INTLIT(2))
    p.expression shouldBe IntLit(1)
    p.expression shouldBe IntLit(-2)

    // (1 + ((-2 * 3) % 4))
    // 1 +
    // -2 *
    // 3 %
    // 4
    parser(INTLIT(1), PLUS, NEWLINE, MINUS, INTLIT(2), TIMES, NEWLINE, INTLIT(3), MODULO, NEWLINE, INTLIT(4))
      .expression shouldBe Plus(IntLit(1), Modulo(Times(IntLit(-2), IntLit(3)), IntLit(4)))

    test("Illegal cases") {
      // 1
      // as Double
      var p = parser(INTLIT(1), NEWLINE, AS, ID("Double"))
      p.expression shouldBe IntLit(1)
      a[ParsingException] should be thrownBy p.expression

      // 1
      // * 2
      p = parser(INTLIT(1), NEWLINE, TIMES, INTLIT(2))
      p.expression shouldBe IntLit(1)
      a[ParsingException] should be thrownBy p.expression
    }
  }

  //------------------------------------------------------------------------------------
  //--- Terms
  //------------------------------------------------------------------------------------

  it should "parse array literals" in {
    parser(LBRACKET, RBRACKET).expression shouldBe ArrayLit(Nil)
    parser(LBRACKET, INTLIT(1), RBRACKET).expression shouldBe ArrayLit(List(IntLit(1)))

    // [1, 2, 3 ]
    parser(LBRACKET, INTLIT(1), COMMA, INTLIT(2), COMMA, INTLIT(3), RBRACKET)
      .expression shouldBe ArrayLit(List(IntLit(1), IntLit(2), IntLit(3)))

    // [
    //    1,
    //    2,
    //    3,
    // ]
    parser(LBRACKET, NEWLINE, INTLIT(1), COMMA, NEWLINE, INTLIT(2), COMMA, NEWLINE, INTLIT(3), COMMA, NEWLINE, RBRACKET)
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
    // super.a
    parser(SUPER, DOT, ID("a")).expression shouldBe NormalAccess(Super(None), VariableID("a"))

    // super.A(1)
    parser(SUPER, DOT, ID("A"), LPAREN, INTLIT(1), RPAREN)
      .expression shouldBe NormalAccess(Super(None), MethodCall("A", List(IntLit(1))))

    // super<A>.a(1)
    parser(SUPER, LESSTHAN, ID("A"), GREATERTHAN, DOT, ID("a"), LPAREN, INTLIT(1), RPAREN)
      .expression shouldBe NormalAccess(Super(Some("A")), MethodCall("a", List(IntLit(1))))
  }

  it should "parse new expressions" in {
    // new A()
    parser(NEW, ID("A"), LPAREN, RPAREN).expression shouldBe New(ClassID("A"), Nil)

    // new A(1)
    parser(NEW, ID("A"), LPAREN, INTLIT(1), RPAREN).expression shouldBe New(ClassID("A"), List(IntLit(1)))

    // new A(
    //    1,
    //    2,
    //    3,
    // )
    parser(
      NEW, ID("A"), LPAREN, NEWLINE, INTLIT(1), COMMA, NEWLINE, INTLIT(2), COMMA, NEWLINE, INTLIT(3), COMMA, NEWLINE,
      RPAREN
    ).expression shouldBe New(ClassID("A"), List(IntLit(1), IntLit(2), IntLit(3)))

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
      NEW, ID("A"), QUESTIONMARK, LBRACKET, INTLIT(1), RBRACKET, QUESTIONMARK, LBRACKET, INTLIT(2), RBRACKET,
      QUESTIONMARK, LBRACKET, INTLIT(3), RBRACKET
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
    parser(ID("A"), DOT, ID("b")).expression shouldBe NormalAccess(VariableID("A"), VariableID("b"))
    parser(ID("A"), DOT, ID("b"), LPAREN, RPAREN).expression shouldBe
      NormalAccess(VariableID("A"), MethodCall("b", Nil))

    parser(ID("A"), SAFEACCESS, ID("b")).expression shouldBe SafeAccess(VariableID("A"), VariableID("b"))
    parser(ID("A"), SAFEACCESS, ID("b"), LPAREN, RPAREN).expression shouldBe
      SafeAccess(VariableID("A"), MethodCall("b", Nil))
  }

  it should "parse indexing expressions" in {
    parser(ID("x"), LBRACKET, INTLIT(1), RBRACKET).expression shouldBe ArrayRead(VariableID("x"), IntLit(1))
    parser(ID("x"), LBRACKET, INTLIT(1), PLUS, INTLIT(1), RBRACKET)
      .expression shouldBe ArrayRead(VariableID("x"), Plus(IntLit(1), IntLit(1)))
  }

  it should "parse slicing expressions" in {
    // x[:]
    parser(ID("x"), LBRACKET, COLON, RBRACKET)
      .expression shouldBe ArraySlice(VariableID("x"), None, None, None)

    // x[::]
    parser(ID("x"), LBRACKET, COLON, COLON, RBRACKET)
      .expression shouldBe ArraySlice(VariableID("x"), None, None, None)

    // x[1:]
    parser(ID("x"), LBRACKET, INTLIT(1), COLON, RBRACKET)
      .expression shouldBe ArraySlice(VariableID("x"), IntLit(1), None, None)

    // x[:1]
    parser(ID("x"), LBRACKET, COLON, INTLIT(1), RBRACKET)
      .expression shouldBe ArraySlice(VariableID("x"), None, IntLit(1), None)

    // x[1::]
    parser(ID("x"), LBRACKET, INTLIT(1), COLON, COLON, RBRACKET)
      .expression shouldBe ArraySlice(VariableID("x"), IntLit(1), None, None)

    // x[:1:]
    parser(ID("x"), LBRACKET, COLON, INTLIT(1), COLON, RBRACKET)
      .expression shouldBe ArraySlice(VariableID("x"), None, IntLit(1), None)

    // x[::1]
    parser(ID("x"), LBRACKET, COLON, COLON, INTLIT(1), RBRACKET)
      .expression shouldBe ArraySlice(VariableID("x"), None, None, IntLit(1))

    // x[1:1]
    parser(ID("x"), LBRACKET, INTLIT(1), COLON, INTLIT(1), RBRACKET)
      .expression shouldBe ArraySlice(VariableID("x"), IntLit(1), IntLit(1), None)

    // x[1:1:]
    parser(ID("x"), LBRACKET, INTLIT(1), COLON, INTLIT(1), COLON, RBRACKET)
      .expression shouldBe ArraySlice(VariableID("x"), IntLit(1), IntLit(1), None)

    // x[1::1]
    parser(ID("x"), LBRACKET, INTLIT(1), COLON, COLON, INTLIT(1), RBRACKET)
      .expression shouldBe ArraySlice(VariableID("x"), IntLit(1), None, IntLit(1))

    // x[:1:1]
    parser(ID("x"), LBRACKET, COLON, INTLIT(1), COLON, INTLIT(1), RBRACKET)
      .expression shouldBe ArraySlice(VariableID("x"), None, IntLit(1), IntLit(1))

    // x[1:1:1]
    parser(ID("x"), LBRACKET, INTLIT(1), COLON, INTLIT(1), COLON, INTLIT(1), RBRACKET)
      .expression shouldBe ArraySlice(VariableID("x"), IntLit(1), IntLit(1), IntLit(1))
  }

  it should "parse extract nullable expressions" in {
    parser(ID("x"), EXTRACTNULLABLE).expression shouldBe ExtractNullable(VariableID("x"))

    parser(INTLIT(1), PLUS, ID("x"), EXTRACTNULLABLE).expression shouldBe Plus(IntLit(1), ExtractNullable(VariableID("x")))
  }

  //------------------------------------------------------------------------------------
  //--- Misc
  //------------------------------------------------------------------------------------

  it should "parse types" in {
    // A
    parser(ID("A")).tpe shouldBe ClassID("A")

    // A::B
    parser(ID("A"), COLON, COLON, ID("B")).tpe shouldBe ClassID("A::B")

    // A<B>
    parser(ID("A"), LESSTHAN, ID("B"), GREATERTHAN).tpe shouldBe ClassID("A", List(ClassID("B")))

    // A?
    parser(ID("A"), QUESTIONMARK).tpe shouldBe NullableType(ClassID("A"))

    // A[]
    parser(ID("A"), LBRACKET, RBRACKET).tpe shouldBe ArrayType(ClassID("A"))

    // A?[]?[][]?
    parser(ID("A"), QUESTIONMARK, LBRACKET, RBRACKET, QUESTIONMARK, LBRACKET, RBRACKET, LBRACKET, RBRACKET, QUESTIONMARK)
      .tpe shouldBe NullableType(ArrayType(ArrayType(NullableType(ArrayType(NullableType(ClassID("A")))))))

    // A<B<C, D>, E>
    parser(ID("A"), LESSTHAN, ID("B"), LESSTHAN, ID("C"), COMMA, ID("D"), GREATERTHAN, COMMA, ID("E"), GREATERTHAN)
      .classType shouldBe ClassID(
      "A",
      List(
        ClassID("B", List(ClassID("C"), ClassID("D"))),
        ClassID("E")
      )
    )

    // A<B<C<D<E<F>>>>>
    parser(
      ID("A"), LESSTHAN, ID("B"), LESSTHAN, ID("C"), LESSTHAN, ID("D"), LESSTHAN, ID("E"), LESSTHAN, ID("F"), RSHIFT,
      RSHIFT, GREATERTHAN
    ).classType shouldBe ClassID("A", List(
      ClassID("B", List(
        ClassID("C", List(
          ClassID("D", List(
            ClassID("E", List(
              ClassID("F")
            ))
          ))
        ))
      ))
    ))

    // A<B?, C<D>?[], D<E?, F?[]>>
    parser(
      ID("A"), LESSTHAN, ID("B"), QUESTIONMARK, COMMA, ID("C"), LESSTHAN, ID("D"), GREATERTHAN, QUESTIONMARK, LBRACKET,
      RBRACKET, COMMA, ID("D"), LESSTHAN, ID("E"), QUESTIONMARK, COMMA, ID("F"), QUESTIONMARK, LBRACKET, RBRACKET,
      RSHIFT
    ).classType shouldBe ClassID(
      "A",
      List(
        NullableType(ClassID("B")),
        ArrayType(
          NullableType(
            ClassID("C", List(ClassID("D")))
          )
        ),
        ClassID("D", List(
          NullableType(ClassID("E")),
          ArrayType(NullableType(ClassID("F")))
        ))
      )
    )

    // A<
    //    B,
    //    C,
    //    D,
    // >
    parser(
      ID("A"), LESSTHAN, NEWLINE, ID("B"), COMMA, NEWLINE, ID("C"), COMMA, NEWLINE, ID("D"), COMMA, NEWLINE, GREATERTHAN
    ).classType shouldBe ClassID("A", List(ClassID("B"), ClassID("C"), ClassID("D")))

    // A<B<C>
    a[ParsingException] should be thrownBy parser(ID("A"), LESSTHAN, ID("B"), LESSTHAN, ID("C"), GREATERTHAN).classType
  }

  it should "parse a formal" in {
    parser(ID("x"), COLON, ID("X")).formal shouldBe Formal("X", "x")
  }

  it should "parse class type identifiers" in {
    parser(ID("A")).classTypeIdentifier shouldBe ClassID("A")

    // A<>
    parser(ID("A"), LESSTHAN, GREATERTHAN).classTypeIdentifier shouldBe ClassID("A")

    // A<B>
    parser(ID("A"), LESSTHAN, ID("B"), GREATERTHAN).classTypeIdentifier shouldBe ClassID("A", List(ClassID("B")))

    // A<B, C, D>
    parser(ID("A"), LESSTHAN, ID("B"), COMMA, ID("C"), COMMA, ID("D"), GREATERTHAN)
      .classTypeIdentifier shouldBe ClassID("A", List(ClassID("B"), ClassID("C"), ClassID("D")))

    // A<
    //    B,
    //    C,
    //    D,
    // >
    parser(
      ID("A"), LESSTHAN, NEWLINE, ID("B"), COMMA, NEWLINE, ID("C"), COMMA, NEWLINE, ID("D"), COMMA, NEWLINE, GREATERTHAN
    ).classTypeIdentifier shouldBe ClassID("A", List(ClassID("B"), ClassID("C"), ClassID("D")))
  }

  it should "parse nested template parameters" in {
    // A<B<T, Int>>
    parser(ID("A"), LESSTHAN, ID("B"), LESSTHAN, ID("T"), COMMA, ID("Int"), RSHIFT)
      .tpe shouldBe ClassID("A", ClassID("B", ClassID("T") :: ClassID("Int") :: Nil) :: Nil)

    // A<B<C<D>>>
    parser(ID("A"), LESSTHAN, ID("B"), LESSTHAN, ID("C"), LESSTHAN, ID("D"), RSHIFT, GREATERTHAN)
      .tpe shouldBe ClassID("A", ClassID("B", ClassID("C", ClassID("D") :: Nil) :: Nil) :: Nil)

    // A<B<C<D<E>>>>
    parser(ID("A"), LESSTHAN, ID("B"), LESSTHAN, ID("C"), LESSTHAN, ID("D"), LESSTHAN, ID("E"), RSHIFT, RSHIFT)
      .tpe shouldBe ClassID("A", ClassID("B", ClassID("C", ClassID("D", ClassID("E") :: Nil) :: Nil) :: Nil) :: Nil)

    // A<B<C<D>>, B<C<D>>>
    parser(ID("A"), LESSTHAN,
      ID("B"), LESSTHAN, ID("C"), LESSTHAN, ID("D"), RSHIFT, COMMA,
      ID("B"), LESSTHAN, ID("C"), LESSTHAN, ID("D"), RSHIFT, GREATERTHAN)
      .tpe shouldBe ClassID("A",
      ClassID("B", ClassID("C", ClassID("D") :: Nil) :: Nil) ::
        ClassID("B", ClassID("C", ClassID("D") :: Nil) :: Nil) :: Nil)

    // A<
    //    B<T, C<Int>>,
    //    B<T, C<Int>>,
    // >
    parser(ID("A"), LESSTHAN, NEWLINE,
      ID("B"), LESSTHAN, ID("T"), COMMA, ID("C"), LESSTHAN, ID("Int"), RSHIFT, COMMA, NEWLINE,
      ID("B"), LESSTHAN, ID("T"), COMMA, ID("C"), LESSTHAN, ID("Int"), RSHIFT, COMMA, NEWLINE,
      GREATERTHAN)
      .tpe shouldBe ClassID("A",
      ClassID("B", ClassID("T") :: ClassID("C", ClassID("Int") :: Nil) :: Nil) ::
        ClassID("B", ClassID("T") :: ClassID("C", ClassID("Int") :: Nil) :: Nil) :: Nil)

    // Def MyMethod(a: Map<String, Vector<Int>>, b: Map<String, Vector<Int>>, c: Map<String, Vector<Int>>)
    parser(PUBDEF, ID("MyMethod"), LPAREN,
      ID("a"), COLON, ID("Map"), LESSTHAN, ID("String"), COMMA, ID("Vector"), LESSTHAN, ID("Int"), RSHIFT, COMMA,
      ID("b"), COLON, ID("Map"), LESSTHAN, ID("String"), COMMA, ID("Vector"), LESSTHAN, ID("Int"), RSHIFT, COMMA,
      ID("c"), COLON, ID("Map"), LESSTHAN, ID("String"), COMMA, ID("Vector"), LESSTHAN, ID("Int"), RSHIFT,
      RPAREN)
      .methodDeclaration(Nil) shouldBe MethodDecl("MyMethod",
      modifiers = Set(Public()),
      retType = None,
      args = List(
        Formal(ClassID("Map", ClassID("String") :: ClassID("Vector", ClassID("Int") :: Nil) :: Nil), "a"),
        Formal(ClassID("Map", ClassID("String") :: ClassID("Vector", ClassID("Int") :: Nil) :: Nil), "b"),
        Formal(ClassID("Map", ClassID("String") :: ClassID("Vector", ClassID("Int") :: Nil) :: Nil), "c")),
      stat = None
    )

    // Def MyMethod(
    //     a: Map<String, Vector<Int>>,
    //     b: Map<String, Vector<Int>>,
    //     c: Map<String, Vector<Int>>,
    // )
    parser(PUBDEF, ID("MyMethod"), LPAREN, NEWLINE,
      ID("a"), COLON, ID("Map"), LESSTHAN, ID("String"), COMMA, ID("Vector"), LESSTHAN, ID("Int"), RSHIFT, COMMA, NEWLINE,
      ID("b"), COLON, ID("Map"), LESSTHAN, ID("String"), COMMA, ID("Vector"), LESSTHAN, ID("Int"), RSHIFT, COMMA, NEWLINE,
      ID("c"), COLON, ID("Map"), LESSTHAN, ID("String"), COMMA, ID("Vector"), LESSTHAN, ID("Int"), RSHIFT, COMMA, NEWLINE,
      RPAREN)
      .methodDeclaration(Nil) shouldBe MethodDecl("MyMethod",
      modifiers = Set(Public()),
      retType = None,
      args = List(
        Formal(ClassID("Map", ClassID("String") :: ClassID("Vector", ClassID("Int") :: Nil) :: Nil), "a"),
        Formal(ClassID("Map", ClassID("String") :: ClassID("Vector", ClassID("Int") :: Nil) :: Nil), "b"),
        Formal(ClassID("Map", ClassID("String") :: ClassID("Vector", ClassID("Int") :: Nil) :: Nil), "c")),
      stat = None
    )
  }

  private implicit val formatter: Formatter = testFormatter(useColor = false)
  private val errorStringContext = ErrorStringContext()
  private val ctx = Context(mock[Reporter], PrettyOutputHandler())

  private def parser(tokens: Any*) = Parser(ctx, errorStringContext, createTokenStream(tokens))

  private def createTokenStream(tokenValues: Seq[Any]): TokenStream = {
    val tokens = tokenValues.map {
      case token: Token    => token
      case kind: TokenKind => new Token(kind)
      case _               => ???
    } :+ new Token(EOF)

    tokens.foreach { _.setPos(StringSource("", "ParsingSpec"), 0, 0, 0, 0) }
    TokenStream(tokens)
  }
}
