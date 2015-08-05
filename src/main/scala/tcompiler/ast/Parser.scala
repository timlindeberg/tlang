package tcompiler
package ast

import tcompiler.ast.Trees._
import tcompiler.lexer.Tokens._
import tcompiler.lexer._
import tcompiler.utils._

import scala.collection.mutable.ArrayBuffer

object Parser extends Pipeline[Iterator[Token], Program] {

  def run(ctx: Context)(tokens: Iterator[Token]): Program = {
    val astBuilder = new ASTBuilder(ctx, tokens)
    astBuilder.parseGoal()
  }

}


class ASTBuilder(ctx: Context, tokens: Iterator[Token]) {
  import ctx.reporter._

  private var currentToken: Token = new Token(BAD)

  /**
   * <goal> ::= [ <mainObject> ] { <classDeclaration> } <EOF>
   */
  def parseGoal() = {
    readToken()
    val pos = currentToken
    val pack = optional(packageDecl, PACKAGE)
    val imp = untilNot(importDecl, IMPORT)
    val main = optional(mainObject, MAIN)
    val classes = until(classDeclaration, EOF)
    Program(pack, imp, main, classes).setPos(pos)
  }

  /**
   * <packageDecl> ::= package <identifier> { . <identifier> }
   */
  def packageDecl() = {
    val pos = currentToken
    val identifiers = nonEmptyList(identifier, DOT)
    eat(SEMICOLON)
    Package(identifiers).setPos(pos)
  }

  /**
   * <importDecl> ::= import [ "<" ] <identifier> { . ( <identifier> | * ) } [ ">" ]
   */
  def importDecl(): Import = {
    val pos = currentToken
    eat(IMPORT)
    val ids = new ArrayBuffer[Identifier]()

    if (currentToken.kind == LESSTHAN) {
      eat(LESSTHAN)
      ids += identifier()
      while (currentToken.kind == DOT) {
        eat(DOT)
        ids += identifier()
      }
      eat(GREATERTHAN, SEMICOLON)
      return GenericImport(ids.toList).setPos(pos)
    }
    ids += identifier()
    while (currentToken.kind == DOT) {
      eat(DOT)
      currentToken.kind match {
        case TIMES =>
          eat(TIMES, SEMICOLON)
          return WildCardImport(ids.toList).setPos(pos)
        case _     => ids += identifier
      }
    }
    eat(SEMICOLON)
    RegularImport(ids.toList).setPos(pos)
  }

  /**
   * <mainObject> ::= object <identifier> "{" def main (): Unit = "{" { <statement> } "}" "}"
   */
  def mainObject(): MainObject = {
    val pos = currentToken
    val id = identifier()
    eat(EQSIGN, LBRACE)
    val stmts = until(statement, RBRACE)
    eat(RBRACE)
    MainObject(id, stmts).setPos(pos)
  }

  /**
   * <classDeclaration> ::= class <classIdentifier>
   * [ extends <classIdentifier> ] "{" { <varDeclaration> } { <methodDeclaration> } "}"
   */
  def classDeclaration(): ClassDecl = {
    val pos = currentToken
    eat(CLASS)
    val id = classTypeIdentifier()
    val parent = optional(classIdentifier, EXTENDS)
    eat(LBRACE)
    val vars = untilNot(varDeclaration, PUBVAR, PRIVVAR)
    val methods = untilNot(() => methodDeclaration(id.value), PRIVDEF, PUBDEF)
    eat(RBRACE)
    InternalClassDecl(id, parent, vars, methods).setPos(pos)
  }

  /**
   * <varDeclaration> ::= (Var | var) <modifiers> <identifier> ":" <tpe> [ = <expression> ] ";"
   */
  def varDeclaration(): VarDecl = {
    val pos = currentToken
    val mods = modifiers(PRIVVAR, PUBVAR)
    val id = identifier()
    eat(COLON)
    val typ = tpe()
    val init = optional(expression, EQSIGN)
    eat(SEMICOLON)
    VarDecl(typ, id, init, mods).setPos(pos)
  }

  /**
   * <formal> ::= <identifier> ":" <tpe>
   */
  def formal(): Formal = {
    val pos = currentToken
    val id = identifier()
    eat(COLON)
    val typ = tpe()
    Formal(typ, id).setPos(pos)
  }

  /**
   * <methodDeclaration> ::= (Def | def [ protected ] ) ( <constructor> | <binaryOperator> | <method> )
   */
  def methodDeclaration(className: String): FuncTree = {
    val pos = currentToken
    val mods = modifiers(PRIVDEF, PUBDEF)
    val func = currentToken.kind match {
      case IDKIND => method(mods)
      case NEW    => constructor(mods, className)
      case _      => operator(mods)
      //case  => //unaryOperator(access)
    }
    func.setPos(pos)
  }

  /**
   * <method> ::= <identifier> "(" [ <formal> { "," <formal> } ] "): " (<tpe> | Unit) "= {" { <varDeclaration> } { <statement> } "}"
   */
  def method(modifiers: Set[Modifier]): MethodDecl = {
    val id = identifier()
    eat(LPAREN)
    val args = commaList(formal)
    eat(RPAREN)
    eat(COLON)
    val retType = if (currentToken.kind == UNIT) {
      val pos = currentToken
      eat(UNIT)
      UnitType().setPos(pos)
    } else {
      tpe()
    }
    eat(EQSIGN, LBRACE)
    val vars = untilNot(varDeclaration, PUBVAR, PRIVVAR)
    val stmts = until(statement, RBRACE)
    eat(RBRACE)
    MethodDecl(retType, id, args, vars, stmts, modifiers)
  }

  /**
   * <constructor> ::= new "(" [ <formal> { "," <formal> } ] ")"  = {" { <varDeclaration> } { <statement> } "}"
   */
  def constructor(modifiers: Set[Modifier], className: String): ConstructorDecl = {
    eat(NEW)
    eat(LPAREN)
    val args = commaList(formal)
    eat(RPAREN)
    eat(EQSIGN, LBRACE)
    val vars = untilNot(varDeclaration, PUBVAR, PRIVVAR)
    val stmts = until(statement, RBRACE)
    eat(RBRACE)
    ConstructorDecl(new Identifier(className), args, vars, stmts, modifiers)
  }

  /**
   * <operator> ::= ( + | - | * | / | % | / | "|" | ^ | << | >> | < | <= | > | >= | ! | ~ | ++ | -- ) "(" <formal> [ "," <formal> ] "): <tpe>  = {" { <varDeclaration> } { <statement> } "}"
   */
  def operator(modifiers: Set[Modifier]): OperatorDecl = {
    val pos = currentToken
    def binaryOperator(constructor: (ExprTree, ExprTree) => ExprTree): (ExprTree, List[Formal], Set[Modifier]) = {
      eat(currentToken.kind)
      val operatorType = constructor(Empty(), Empty())
      eat(LPAREN)
      val f1 = formal()
      eat(COMMA)
      val f2 = formal()
      (operatorType, List(f1, f2), modifiers + Static)
    }

    def unaryOperator(constructor: (ExprTree) => ExprTree): (ExprTree, List[Formal], Set[Modifier]) = {
      eat(currentToken.kind)
      val operatorType = constructor(Empty())
      eat(LPAREN)
      (operatorType, List(formal()), modifiers + Static)
    }

    def incrementDecrement(constructor: (Identifier) => ExprTree): (ExprTree, List[Formal], Set[Modifier]) = {
      eat(currentToken.kind)
      eat(LPAREN)
      (constructor(new Identifier("")), List(formal()), modifiers + Static)
    }

    val (operatorType, args, newModifiers) = currentToken.kind match {
      case PLUS              => binaryOperator(Plus)
      case MINUS             => binaryOperator(Minus)
      case TIMES             => binaryOperator(Times)
      case DIV               => binaryOperator(Div)
      case MODULO            => binaryOperator(Modulo)
      case LOGICAND          => binaryOperator(LogicAnd)
      case LOGICOR           => binaryOperator(LogicOr)
      case LOGICXOR          => binaryOperator(LogicXor)
      case LEFTSHIFT         => binaryOperator(LeftShift)
      case RIGHTSHIFT        => binaryOperator(RightShift)
      case LESSTHAN          => binaryOperator(LessThan)
      case LESSTHANEQUALS    => binaryOperator(LessThanEquals)
      case GREATERTHAN       => binaryOperator(GreaterThan)
      case GREATERTHANEQUALS => binaryOperator(GreaterThanEquals)
      case EQUALS            => binaryOperator(Equals)
      case NOTEQUALS         => binaryOperator(NotEquals)
      case LOGICNOT          => unaryOperator(LogicNot)
      case BANG              => unaryOperator(Not)
      case INCREMENT         => incrementDecrement(PreIncrement)
      case DECREMENT         => incrementDecrement(PreDecrement)
      case LBRACKET          =>
        eat(LBRACKET, RBRACKET)
        currentToken.kind match {
          case EQSIGN =>
            if (modifiers.contains(Static))
              ErrorStaticIndexingOperator("[]=", pos)
            val operatorType = ArrayAssign(new Identifier(""), Empty(), Empty())
            eat(EQSIGN, LPAREN)
            val f1 = formal()
            eat(COMMA)
            val f2 = formal()
            (operatorType, List(f1, f2), modifiers)
          case LPAREN =>
            if (modifiers.contains(Static))
              ErrorStaticIndexingOperator("[]", pos)
            val operatorType = ArrayRead(new Identifier(""), Empty())
            eat(LPAREN)
            val f1 = formal()
            (operatorType, List(f1), modifiers)
          case _      => FatalWrongToken(EQSIGN, LPAREN)
        }
      case _                 =>
        FatalWrongToken(PLUS, MINUS, TIMES, DIV, MODULO, LOGICAND, LOGICOR, LOGICXOR, LEFTSHIFT, RIGHTSHIFT, LESSTHAN,
          LESSTHANEQUALS, GREATERTHAN, GREATERTHANEQUALS, EQUALS, NOTEQUALS, INCREMENT, DECREMENT, LOGICNOT, BANG, LBRACKET)
    }
    eat(RPAREN)
    eat(COLON)

    val retType = if (operatorType.isInstanceOf[ArrayAssign]) {
      eat(UNIT)
      UnitType()
    } else {
      tpe()
    }
    eat(EQSIGN, LBRACE)
    val vars = untilNot(varDeclaration, PUBVAR, PRIVVAR)
    val stmts = until(statement, RBRACE)
    eat(RBRACE)
    OperatorDecl(operatorType, retType, args, vars, stmts, newModifiers)
  }


  /**
   * <modifiers> ::= (pub | priv [ protected ] ) [ static ]
   */
  def modifiers(priv: TokenKind, pub: TokenKind): Set[Modifier] = {
    val access = accessRights(priv, pub)
    var modifiers: Set[Modifier] = Set(access)
    currentToken.kind match {
      case STATIC =>
        eat(STATIC)
        modifiers = modifiers + Static
      case _      =>
    }
    modifiers
  }

  /**
   * <tpe> ::= ( Int | Long | Float | Double | Bool | Char | String | <classIdentifier> ) { "[]" }
   */
  def tpe(): TypeTree = {
    val pos = currentToken
    val tpe = currentToken.kind match {
      case INT     =>
        eat(INT)
        IntType()
      case LONG    =>
        eat(LONG)
        LongType()
      case FLOAT   =>
        eat(FLOAT)
        FloatType()
      case DOUBLE  =>
        eat(DOUBLE)
        DoubleType()
      case BOOLEAN =>
        eat(BOOLEAN)
        BooleanType()
      case CHAR    =>
        eat(CHAR)
        CharType()
      case STRING  =>
        eat(STRING)
        StringType()
      case _       => classIdentifier()
    }
    var e = tpe
    while (currentToken.kind == LBRACKET) {
      e.setPos(pos)
      eat(LBRACKET, RBRACKET)
      e = ArrayType(e)
    }
    e.setPos(pos)
  }

  /**
   * <statement> ::= "{" { <statement> } "}
   * | if"(" <expression> ")" <statement> [ else <statement> ]
   * | while"(" <expression> ")" <statement>
   * | for "("[ <assignment> { "," <assignment> } ";" <expression> ";" [ <expression> { "," <expression> } ] " ) <statement>
   * | println"(" <expression> ");"
   * | return [ <expression> ] ";"
   * | <identifier> "=" <expression> ";"
   * | <identifier> "+=" <expression> ";"
   * | <identifier> "-=" <expression> ";"
   * | <identifier> "*=" <expression> ";"
   * | <identifier> "/=" <expression> ";"
   * | <identifier> "%=" <expression> ";"
   * | <identifier> "&=" <expression> ";"
   * | <identifier> "|=" <expression> ";"
   * | <identifier> "^=" <expression> ";"
   * | <identifier> "<<=" <expression> ";"
   * | <identifier> ">>=" <expression> ";"
   * | <identifier> "[" <expression> "]" "=" <expression> ";"
   * | <identifier> "++"
   * | <identifier> "--"
   * | "++" <identifier>
   * | "--" <identifier>
   * | <expression>"."<identifier>"(" [ <expression> { "," <expression> } ] [ "."<identifier>"(" [ <expression> { "," <expression> } ] }
   */
  def statement(): StatTree = {
    val pos = currentToken
    val tree = currentToken.kind match {
      case LBRACE  =>
        eat(LBRACE)
        val stmts = until(statement, RBRACE)
        eat(RBRACE)
        Block(stmts)
      case IF      =>
        eat(IF, LPAREN)
        val expr = expression()
        eat(RPAREN)
        val stmt = statement()
        val els = optional(statement, ELSE)
        If(expr, stmt, els)
      case WHILE   =>
        eat(WHILE, LPAREN)
        val expr = expression()
        eat(RPAREN)
        While(expr, statement())
      case FOR     =>
        eat(FOR, LPAREN)
        // Initiation
        val init = commaList(() => {
          val id = identifier()
          currentToken.kind match {
            case EQSIGN | PLUSEQ | MINUSEQ |
                 DIVEQ | MODEQ | ANDEQ | OREQ |
                 XOREQ | LEFTSHIFTEQ | RIGHTSHIFTEQ =>
            case _                                  => FatalWrongToken(EQSIGN, PLUSEQ, MINUSEQ, MULEQ, DIVEQ, MODEQ, ANDEQ, OREQ, XOREQ, LEFTSHIFTEQ, RIGHTSHIFTEQ)
          }
          assignment(Some(id)).asInstanceOf[Assign]
        }, SEMICOLON)
        eat(SEMICOLON)
        // Condition
        val condition = expression()
        eat(SEMICOLON)
        // Incrementation
        val post = commaList(() => currentToken.kind match {
          case INCREMENT =>
            eat(INCREMENT)
            PreIncrement(identifier())
          case DECREMENT =>
            eat(DECREMENT)
            PreDecrement(identifier())
          case IDKIND    =>
            val id = identifier()
            currentToken.kind match {
              case PLUSEQ | MINUSEQ | DIVEQ |
                   MODEQ | ANDEQ | OREQ | XOREQ |
                   LEFTSHIFTEQ | RIGHTSHIFTEQ =>
                assignment(Some(id)).asInstanceOf[Assign]
              case INCREMENT                  =>
                eat(INCREMENT)
                PostIncrement(id)
              case DECREMENT                  =>
                eat(DECREMENT)
                PostDecrement(id)
              case _                          => FatalWrongToken(PLUSEQ, MINUSEQ, MULEQ, DIVEQ, MODEQ, ANDEQ, OREQ, XOREQ, LEFTSHIFTEQ, RIGHTSHIFTEQ, INCREMENT, DECREMENT)
            }
          case _         => FatalWrongToken(INCREMENT, DECREMENT, IDKIND)
        })
        eat(RPAREN)
        For(init, condition, post, statement())
      case PRINT   =>
        eat(PRINT, LPAREN)
        val expr = expression()
        eat(RPAREN, SEMICOLON)
        Print(expr)
      case PRINTLN =>
        eat(PRINTLN, LPAREN)
        val expr = expression()
        eat(RPAREN, SEMICOLON)
        Println(expr)
      case RETURN  =>
        eat(RETURN)
        val expr = if (currentToken.kind != SEMICOLON) Some(expression()) else None
        eat(SEMICOLON)
        Return(expr)
      case _       =>
        val expr = expression()
        eat(SEMICOLON)
        expr match {
          case stat: StatTree => stat
          case _              => FatalInvalidStatement(expr)
        }
    }
    tree.setPos(pos)
  }


  /**
   * <expression> ::= <assignment>
   */
  def expression(): ExprTree = {
    val pos = currentToken
    assignment().setPos(pos)
  }


  /**
   * <assignment> ::= <ternary> [ ( = | += | -= | *= | /= | %= | &= | |= | ^= | <<= | >>= ) <expression> ]
   * | <ternary> [ "[" <expression> "] = " <expression> ]
   */
  def assignment(expr: Option[ExprTree] = None) = {
    val e = if (expr.isDefined) expr.get else ternary()

    def assignment(constructor: Option[(ExprTree, ExprTree) => ExprTree]) = {
      eat(currentToken.kind)

      def assignmentExpr(expr: ExprTree) = if (constructor.isDefined) constructor.get(expr, expression()) else expression()

      e match {
        case ArrayRead(idExpr, index) =>
          idExpr match {
            case id: Identifier => ArrayAssign(id, index, assignmentExpr(e))
            case _              => FatalExpectedIdArrayAssignment(e)
          }
        case FieldRead(obj, id)       => FieldAssign(obj, id, assignmentExpr(e))
        case id: Identifier           => Assign(id, assignmentExpr(id))
        case _                        => FatalExpectedIdAssignment(e)
      }
    }

    currentToken.kind match {
      case EQSIGN       => assignment(None)
      case PLUSEQ       => assignment(Some(Plus))
      case MINUSEQ      => assignment(Some(Minus))
      case MULEQ        => assignment(Some(Times))
      case DIVEQ        => assignment(Some(Div))
      case MODEQ        => assignment(Some(Modulo))
      case ANDEQ        => assignment(Some(LogicAnd))
      case OREQ         => assignment(Some(LogicOr))
      case XOREQ        => assignment(Some(LogicXor))
      case LEFTSHIFTEQ  => assignment(Some(LeftShift))
      case RIGHTSHIFTEQ => assignment(Some(RightShift))
      case _            => e
    }
  }

  /** <ternary> ::= <or> [ ? <or> : <or> ] */
  def ternary() = {
    var e = or()
    val pos = currentToken
    if (currentToken.kind == QUESTIONMARK) {
      eat(QUESTIONMARK)
      val thn = or()
      eat(COLON)
      val els = or()
      e = Ternary(e, thn, els).setPos(pos)
    }
    e
  }

  /** <or> ::= <and> { || <and> } */
  def or() = leftAssociative(and, OR)

  /** <and> ::= <logicOr> { && <logicOr> } */
  def and() = leftAssociative(logicOr, AND)

  /** <logicOr> ::= <logicXor> { | <logicXor> } */
  def logicOr() = leftAssociative(logicXor, LOGICOR)

  /** <logicXor> ::= <logicAnd> { ^ <logicAnd> } */
  def logicXor() = leftAssociative(logicAnd, LOGICXOR)

  /** <logicAnd> ::= <eqNotEq> { & <eqNotEq> } */
  def logicAnd() = leftAssociative(eqNotEq, LOGICAND)

  /** <eqNotEq> ::= <instOf> { ( == | != ) <instOf> } */
  def eqNotEq() = leftAssociative(instOf, EQUALS, NOTEQUALS)

  /** <instOf> ::= <comparison> { inst <identifier> } */
  def instOf() = {
    var e = comparison()
    while (currentToken.kind == INSTANCEOF) {
      eat(INSTANCEOF)
      e = Instance(e, identifier())
    }
    e
  }

  /** <comparison> ::= <bitShift> { ( < | <= | > | >= | inst ) <bitShift> } */
  def comparison() = leftAssociative(bitShift, LESSTHAN, LESSTHANEQUALS, GREATERTHAN, GREATERTHANEQUALS)

  /** <bitShift> ::= <plusMinus> { ( << | >> ) <plusMinus> } */
  def bitShift() = leftAssociative(plusMinus, LEFTSHIFT, RIGHTSHIFT)

  /** <plusMinus> ::= <timesDiv> { ( + | - ) <timesDiv> } */
  def plusMinus() = leftAssociative(timesDivMod, PLUS, MINUS)

  /** <timesDivMod> ::= <term> { ( * | / | % ) <term> } */
  def timesDivMod() = leftAssociative(term, TIMES, DIV, MODULO)

  /**
   * <term> ::= <termFirst> [ termRest ]
   */
  def term(): ExprTree = {
    /**
     * <termFirst> ::= "(" <expression> ")"
     * | ! <expression>
     * | - <expression>
     * | -- <identifier>
     * | ++ <identifier>
     * | ~ <identifier>
     * | <intLit>
     * | <stringLit>
     * | <identifier>
     * | <identifier> ++
     * | <identifier> --
     * | <identifier> "(" <expression> { "," <expression> } ")"
     * | true
     * | false
     * | this
     * | new <tpe>"[" <expression> "]"
     * | new <classIdentifier> "(" [ <expression> { "," <expression> } ")"
     */
    def termFirst() = {
      val pos = currentToken
      val tree = currentToken.kind match {
        case LPAREN        =>
          eat(LPAREN)
          val expr = expression()
          eat(RPAREN)
          expr
        case BANG          =>
          eat(BANG)
          Not(term())
        case MINUS         =>
          eat(MINUS)
          currentToken match {
            case x: INTLIT    =>
              eat(INTLITKIND)
              IntLit(-x.value)
            case x: LONGLIT   =>
              eat(LONGLITKIND)
              LongLit(-x.value)
            case x: FLOATLIT  =>
              eat(FLOATLITKIND)
              FloatLit(-x.value)
            case x: DOUBLELIT =>
              eat(DOUBLELITKIND)
              DoubleLit(-x.value)
            case x: CHARLIT   =>
              eat(CHARLITKIND)
              IntLit(-x.value)
            case _            =>
              Negation(term())
          }
        case LOGICNOT      =>
          eat(LOGICNOT)
          LogicNot(term())
        case DECREMENT     =>
          eat(DECREMENT)
          PreDecrement(identifier())
        case INCREMENT     =>
          eat(INCREMENT)
          PreIncrement(identifier())
        case INTLITKIND    =>
          intLit()
        case LONGLITKIND   =>
          longLit()
        case FLOATLITKIND  =>
          floatLit()
        case DOUBLELITKIND =>
          doubleLit()
        case CHARLITKIND   =>
          charLit()
        case STRLITKIND    =>
          stringLit()
        case IDKIND        =>
          val id = identifier()
          currentToken.kind match {
            case INCREMENT =>
              eat(INCREMENT)
              PostIncrement(id)
            case DECREMENT =>
              eat(DECREMENT)
              PostDecrement(id)
            case LPAREN    =>
              eat(LPAREN)
              val exprs = commaList(expression)
              eat(RPAREN)
              MethodCall(This(), id, exprs) // Implicit this
            case _         => id
          }
        case TRUE          =>
          eat(TRUE)
          True()
        case FALSE         =>
          eat(FALSE)
          False()
        case THIS          =>
          eat(THIS)
          This()
        case NEW           =>
          eat(NEW)
          def primitiveArray(construct: () => TypeTree) = {
            eat(currentToken.kind, LBRACKET)
            val expr = expression()
            eat(RBRACKET)
            NewArray(construct(), expr)
          }
          currentToken.kind match {
            case INT     => primitiveArray(IntType)
            case LONG    => primitiveArray(LongType)
            case FLOAT   => primitiveArray(FloatType)
            case DOUBLE  => primitiveArray(DoubleType)
            case CHAR    => primitiveArray(CharType)
            case STRING  => primitiveArray(StringType)
            case BOOLEAN => primitiveArray(BooleanType)
            case _       =>
              val id = classIdentifier()
              currentToken.kind match {
                case LPAREN   =>
                  eat(LPAREN)
                  val args = commaList(expression)
                  eat(RPAREN)
                  New(id, args)
                case LBRACKET =>
                  eat(LBRACKET)
                  val expr = expression()
                  eat(RBRACKET)
                  NewArray(id, expr)
                case _        => FatalWrongToken(LPAREN, LBRACKET)
              }

          }
        case _             => FatalWrongToken(LPAREN, BANG, INTLITKIND, STRLITKIND, IDKIND, TRUE, FALSE, THIS, NEW)
      }
      tree.setPos(pos)
    }

    /**
     * <termRest> ::= .length
     * | .<identifier>
     * | .<identifier> "(" <expression> { "," <expression> } ")
     * | "[" <expression> "]"
     * | as <tpe>
     */
    def termRest(lhs: ExprTree): ExprTree = {
      val pos = currentToken
      var e = lhs
      val tokens = List(DOT, LBRACKET, AS)

      while (tokens.contains(currentToken.kind)) {
        e = currentToken.kind match {
          case DOT      =>
            eat(DOT)
            if (currentToken.kind == LENGTH) {
              eat(LENGTH)
              ArrayLength(e)
            } else {
              val id = identifier()
              if (currentToken.kind == LPAREN) {
                eat(LPAREN)
                val exprs = commaList(expression)
                eat(RPAREN)
                MethodCall(e, id, exprs.toList)
              } else {
                FieldRead(e, id)
              }
            }
          case LBRACKET =>
            eat(LBRACKET)
            val expr = expression()
            eat(RBRACKET)
            ArrayRead(e, expr)
          case AS       =>
            eat(AS)
            As(e, tpe())
          case _        => e
        }
      }

      e.setPos(pos)
    }
    termRest(termFirst())
  }

  /**
   * Parses expressions of type
   * E ::= <next> { ( kinds[0] | kinds[1] | ... | kinds[n] ) <next> }.
   * Used to parse left associative expressions. *
   */
  private def leftAssociative(next: () => ExprTree, kinds: TokenKind*): ExprTree = {
    var expr = next()
    while (kinds.contains(currentToken.kind)) {
      kinds.foreach { kind =>
        if (currentToken.kind == kind) {
          val pos = currentToken
          eat(kind)
          expr = ASTBuilder.tokenMap(kind)(expr, next()).setPos(pos)
        }
      }
    }
    expr
  }


  /** Store the current token, as read from the lexer. */

  private def readToken(): Unit = {
    if (tokens.hasNext) {
      // uses nextToken from the Lexer trait
      currentToken = tokens.next()

      // skips bad tokens
      while (currentToken.kind == BAD) {
        currentToken = tokens.next()
      }
    }
  }

  /** ''Eats'' the expected token, or terminates with an error. */
  private def eat(kind: TokenKind*): Unit =
    for (k <- kind) {
      if (currentToken.kind == k) {
        readToken()
      } else {
        FatalWrongToken(k)
      }
    }

  /**
   * Parses the correct access rights given the private token and
   * public token to use.
   */
  private def accessRights(priv: TokenKind, pub: TokenKind) =
    currentToken.kind match {
      case x if x == pub  =>
        eat(pub)
        Public
      case x if x == priv =>
        eat(priv)
        if (currentToken.kind == PROTECTED) {
          eat(PROTECTED)
          Protected
        }
        else Private
      case _              => FatalWrongToken(pub, priv)
    }

  private var usedOneGreaterThan = false

  /**
   * Handles the conflict of generics having multiple ">" signs by
   * treating RIGHTSHIFT (>>) as two ">".
   */
  private def eatRightShiftOrGreaterThan() =
    if (currentToken.kind == RIGHTSHIFT) {
      if (usedOneGreaterThan) {
        eat(RIGHTSHIFT)
      }
      usedOneGreaterThan = !usedOneGreaterThan
    } else {
      eat(GREATERTHAN)
    }

  /**
   * <classTypeIdentifier> ::= <identifier> [ "[" <identifier> { "," <identifier> } "]" ]
   */
  private def classTypeIdentifier(): ClassIdentifier = currentToken match {
    case id: ID =>
      eat(IDKIND)
      val tIds = currentToken.kind match {
        case LESSTHAN =>
          eat(LESSTHAN)
          val tmp = commaList(identifier)
          eatRightShiftOrGreaterThan()
          tmp.map(x => new ClassIdentifier(x.value, List()))
        case _        => List()
      }
      ClassIdentifier(id.value, tIds).setPos(id)
    case _      => FatalWrongToken(IDKIND)
  }

  /**
   * <classIdentifier> ::= <identifier> [ "[" <type> { "," <type> } "]" ]
   */
  private def classIdentifier(): ClassIdentifier = {
    val pos = currentToken
    val ids = nonEmptyList(identifier, DOT)
    val tIds = currentToken.kind match {
      case LESSTHAN =>
        eat(LESSTHAN)
        val tmp = commaList(tpe)
        eatRightShiftOrGreaterThan()
        tmp
      case _        => List()
    }
    ClassIdentifier(ids.map(_.value).mkString("."), tIds).setPos(pos)
  }


  /**
   * <identifier> ::= sequence of letters, digits and underscores, starting with a letter and which is not a keyword
   */
  private def identifier(): Identifier = currentToken match {
    case id: ID =>
      eat(IDKIND)
      Identifier(id.value).setPos(id)
    case _      => FatalWrongToken(IDKIND)
  }

  /**
   * <stringLit> ::= sequence of arbitrary characters, except new lines and "
   */
  private def stringLit(): StringLit = currentToken match {
    case strlit: STRLIT =>
      eat(STRLITKIND)
      StringLit(strlit.value).setPos(strlit)
    case _              => FatalWrongToken(STRLITKIND)
  }

  /**
   * <intLit> ::= sequence of digits, with no leading zeros
   */
  private def intLit(): IntLit = currentToken match {
    case intlit: INTLIT =>
      eat(INTLITKIND)
      IntLit(intlit.value).setPos(intlit)
    case _              => FatalWrongToken(INTLITKIND)
  }

  /**
   * <longLit> ::= sequence of digits, with no leading zeros ending with an 'l'
   */
  private def longLit(): LongLit = currentToken match {
    case longLit: LONGLIT =>
      eat(LONGLITKIND)
      LongLit(longLit.value).setPos(longLit)
    case _                => FatalWrongToken(LONGLITKIND)
  }

  /**
   * <floatLit> ::= sequence of digits, optionally with a single '.' ending with an 'f'
   */
  private def floatLit(): FloatLit = currentToken match {
    case floatLit: FLOATLIT =>
      eat(FLOATLITKIND)
      FloatLit(floatLit.value).setPos(floatLit)
    case _                  => FatalWrongToken(FLOATLITKIND)
  }


  /**
   * <doubleLit> ::= sequence of digits, optionally with a single '.' ending with an 'f'
   */
  private def doubleLit(): DoubleLit = currentToken match {
    case doubleLit: DOUBLELIT =>
      eat(DOUBLELITKIND)
      DoubleLit(doubleLit.value).setPos(doubleLit)
    case _                    => FatalWrongToken(DOUBLELITKIND)
  }

  /**
   * <charLit> ::= sequence of digits, optionally with a single '.' ending with an 'f'
   */
  private def charLit(): CharLit = currentToken match {
    case charLit: CHARLIT =>
      eat(CHARLITKIND)
      CharLit(charLit.value).setPos(charLit)
    case _                => FatalWrongToken(CHARLITKIND)
  }

  /**
   * Parses lists of the form
   * <nonEmptyList> ::= parse { delimiter parse }
   */

  private def nonEmptyList[T](parse: () => T, delimiter: TokenKind): List[T] = {
    val arrBuff = new ArrayBuffer[T]()
    arrBuff += parse()
    while (currentToken.kind == delimiter) {
      eat(delimiter)
      arrBuff += parse()
    }
    arrBuff.toList
  }

  /**
   * Parses a commalist of the form
   * <commaList> ::= [ parse { "," parse } ]
   */
  private def commaList[T](parse: () => T, stopSign: TokenKind = RPAREN): List[T] = {
    if (currentToken.kind == stopSign) {
      List()
    } else {
      val arrBuff = new ArrayBuffer[T]()
      arrBuff += parse()
      while (currentToken.kind == COMMA) {
        eat(COMMA)
        arrBuff += parse()
      }
      arrBuff.toList
    }
  }

  /**
   * Parses an optional of the form
   * <optional> ::= [ parse ] and returns Option
   */
  private def optional[T](parse: () => T, kind: TokenKind): Option[T] = {
    if (currentToken.kind == kind) {
      eat(kind)
      Some(parse())
    } else {
      None
    }
  }

  /**
   * Continues parsing until the given token kind is encountered.
   */
  private def until[T](parse: () => T, kinds: TokenKind*): List[T] = {
    val condition = () => !kinds.contains(currentToken.kind)
    _until(condition, parse)
  }

  /**
   * Continues parsing until a token different from the given token is encountered.
   */
  private def untilNot[T](parse: () => T, kinds: TokenKind*): List[T] = {
    val condition = () => kinds.contains(currentToken.kind)
    _until(condition, parse)
  }

  private def _until[T](condition: () => Boolean, parse: () => T): List[T] = {
    var arrBuff = new ArrayBuffer[T]()
    while (condition()) {
      arrBuff += parse()
    }
    arrBuff.toList
  }

  //---------------------------------------------------------------------------------------
  //  Error messages
  //---------------------------------------------------------------------------------------

  private def ErrorStaticIndexingOperator(name: String, pos: Positioned) =
    error(s"Indexing operator '$name' cannot be declared static!", pos)

  private def FatalInvalidStatement(pos: Positioned) =
    fatal("Not a valid statement, expected println, if, while, assignment, a method call or incrementation/decrementation. ", pos)

  private def FatalExpectedIdArrayAssignment(pos: Positioned) =
    fatal("Expected identifier on left hand side of array assignment.", pos)

  private def FatalExpectedIdAssignment(pos: Positioned) =
    fatal("Expected identifier on left side of assignment.", pos)

  private def FatalWrongToken(kind: TokenKind, more: TokenKind*): Nothing = FatalWrongToken((kind :: more.toList).mkString(" or "), currentToken.toString, currentToken)
  private def FatalWrongToken(expected: String, found: String, pos: Positioned): Nothing =
    fatal(s"Expected $expected, found: $found.", pos)

}

object ASTBuilder {

  private val tokenMap: Map[TokenKind, (ExprTree, ExprTree) => ExprTree] = Map(
    OR -> Or,
    AND -> And,
    LESSTHAN -> LessThan,
    LESSTHANEQUALS -> LessThanEquals,
    GREATERTHAN -> GreaterThan,
    GREATERTHANEQUALS -> GreaterThanEquals,
    EQUALS -> Equals,
    NOTEQUALS -> NotEquals,
    PLUS -> Plus,
    MINUS -> Minus,
    TIMES -> Times,
    DIV -> Div,
    MODULO -> Modulo,
    LEFTSHIFT -> LeftShift,
    RIGHTSHIFT -> RightShift,
    LOGICAND -> LogicAnd,
    LOGICOR -> LogicOr,
    LOGICXOR -> LogicXor
  )

}