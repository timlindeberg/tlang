package tcompiler
package ast

import tcompiler.analyzer.Types.TUnit
import tcompiler.ast.Trees._
import tcompiler.lexer.Tokens._
import tcompiler.lexer._
import tcompiler.utils._

import scala.collection.mutable.ArrayBuffer

object Parser extends Pipeline[List[Token], Program] {

  def run(ctx: Context)(tokens: List[Token]): Program = {
    val astBuilder = new ASTBuilder(ctx, tokens.toArray)
    astBuilder.parseGoal()
  }

}


class ASTBuilder(ctx: Context, tokens: Array[Token]) {

  import ctx.reporter._

  private var currentIndex = 0
  private var currentToken: Token = tokens(currentIndex)

  private def nextToken =
    if (currentToken.kind == NEWLINE) tokens(currentIndex + 1) else currentToken

  private def nextTokenKind = nextToken.kind


  /**
   * <goal> ::= [ <mainObject> ] { <classDeclaration> } <EOF>
   */
  def parseGoal() = {
    val pos = nextToken
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
    val pos = nextToken
    val identifiers = nonEmptyList(identifier, DOT)
    endStatement()
    Package(identifiers).setPos(pos)
  }

  /**
   * <importDecl> ::= import [ "<" ] <identifier> { . ( <identifier> | * ) } [ ">" ]
   */
  def importDecl(): Import = {
    val pos = nextToken
    eat(IMPORT)
    val ids = new ArrayBuffer[Identifier]()

    if (nextTokenKind == LESSTHAN) {
      eat(LESSTHAN)
      ids += identifier()
      while (nextTokenKind == DOT) {
        eat(DOT)
        ids += identifier()
      }
      eat(GREATERTHAN)
      endStatement()
      return GenericImport(ids.toList).setPos(pos)
    }
    ids += identifier()
    while (nextTokenKind == DOT) {
      eat(DOT)
      nextTokenKind match {
        case TIMES =>
          eat(TIMES)
          endStatement()
          return WildCardImport(ids.toList).setPos(pos)
        case _     => ids += identifier
      }
    }
    endStatement()
    RegularImport(ids.toList).setPos(pos)
  }

  /**
   * <mainObject> ::= object <identifier> "{" def main (): Unit = "{" { <statement> } "}" "}"
   */
  def mainObject(): MethodDecl = {
    val pos = nextToken
    val id = identifier()
    eat(EQSIGN)


    val args = List(Formal(ArrayType(StringType()), Identifier("args")))
    val modifiers: Set[Modifier] = Set(Public, Static)
    MethodDecl(Some(UnitType()), id, args, statement(), modifiers).setPos(pos)
  }

  /**
   * <classDeclaration> ::= class <classIdentifier>
   * [ extends <classIdentifier> ] "{" { <varDeclaration> } { <methodDeclaration> } "}"
   */
  def classDeclaration(): ClassDecl = {
    val pos = nextToken
    eat(CLASS)
    val id = classTypeIdentifier()
    val parent = optional(classIdentifier, EXTENDS)
    eat(LBRACE)
    val vars = untilNot(() => {
      val v = fieldDeclaration()
      endStatement()
      v
    }, PUBVAR, PRIVVAR)
    val methods = untilNot(() => methodDeclaration(id.value), PRIVDEF, PUBDEF)
    eat(RBRACE)
    InternalClassDecl(id, parent, vars, methods).setPos(pos)
  }

  /**
   * <fieldDeclaration> ::= (Var | var) <modifiers> <variableEnd>
   */
  def fieldDeclaration(): VarDecl = {
    val pos = nextToken
    varDeclEnd(modifiers(PRIVVAR, PUBVAR), pos)
  }

  /**
   * <localVarDeclaration> ::= var <variableEnd>
   */
  def localVarDeclaration(): VarDecl = {
    val pos = nextToken
    eat(PRIVVAR)
    varDeclEnd(Set(), pos)
  }

  /**
   * <varDeclEnd> ::= <identifier> [ ":" <tpe> ] [ = <expression> ]
   */
  def varDeclEnd(modifiers: Set[Modifier], pos: Positioned): VarDecl = {
    val id = identifier()
    val (typ, init) = nextTokenKind match {
      case COLON =>
        eat(COLON)
        (Some(tpe()), optional(expression, EQSIGN))
      case _     =>
        (None, optional(expression, EQSIGN))
    }
    VarDecl(typ, id, init, modifiers).setPos(pos)
  }

  /**
   * <formal> ::= <identifier> ":" <tpe>
   */
  def formal(): Formal = {
    val pos = nextToken
    val id = identifier()
    eat(COLON)
    val typ = tpe()
    Formal(typ, id).setPos(pos)
  }

  /**
   * <methodDeclaration> ::= (Def | def [ protected ] ) ( <constructor> | <binaryOperator> | <method> )
   */
  def methodDeclaration(className: String): FuncTree = {
    val pos = nextToken
    val mods = modifiers(PRIVDEF, PUBDEF)
    val func = nextTokenKind match {
      case IDKIND => method(mods)
      case NEW    => constructor(mods, className)
      case _      => operator(mods)
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
    val retType = optional(returnType, COLON)
    eat(EQSIGN)

    MethodDecl(retType, id, args, statement(), modifiers)
  }

  /**
   * <constructor> ::= new "(" [ <formal> { "," <formal> } ] ")"  = {" { <varDeclaration> } { <statement> } "}"
   */
  def constructor(modifiers: Set[Modifier], className: String): ConstructorDecl = {
    eat(NEW)
    eat(LPAREN)
    val args = commaList(formal)
    eat(RPAREN)
    eat(EQSIGN)
    val retType = Some(UnitType().setType(TUnit))
    ConstructorDecl(retType, new Identifier(className), args, statement(), modifiers)
  }

  /**
   * <returnType> ::= Unit | <tpe>
   */
  def returnType(): TypeTree =
    if (nextTokenKind == UNIT) {
      val pos = nextToken
      eat(UNIT)
      UnitType().setPos(pos)
    } else {
      tpe()
    }

  /**
   * <operator> ::= ( + | - | * | / | % | / | "|" | ^ | << | >> | < | <= | > | >= | ! | ~ | ++ | -- ) "(" <formal> [ "," <formal> ] "): <tpe>  = {" { <varDeclaration> } { <statement> } "}"
   */
  def operator(modifiers: Set[Modifier]): OperatorDecl = {
    val pos = nextToken
    def binaryOperator(constructor: (ExprTree, ExprTree) => ExprTree): (ExprTree, List[Formal], Set[Modifier]) = {
      eat(nextTokenKind)
      val operatorType = constructor(Empty(), Empty())
      eat(LPAREN)
      val f1 = formal()
      eat(COMMA)
      val f2 = formal()
      (operatorType, List(f1, f2), modifiers + Static)
    }

    def unaryOperator(constructor: (ExprTree) => ExprTree): (ExprTree, List[Formal], Set[Modifier]) = {
      eat(nextTokenKind)
      val operatorType = constructor(Empty())
      eat(LPAREN)
      (operatorType, List(formal()), modifiers + Static)
    }

    val (operatorType, args, newModifiers) = nextTokenKind match {
      case PLUS              => binaryOperator(Plus)
      case MINUS             =>
        eat(MINUS)
        eat(LPAREN)
        val f1 = formal()
        nextTokenKind match {
          case COMMA =>
            eat(COMMA)
            val f2 = formal()
            (Minus(Empty(), Empty()), List(f1, f2), modifiers + Static)
          case _     =>
            (Negation(Empty()), List(f1), modifiers + Static)
        }
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
      case HASH              => unaryOperator(Hash)
      case INCREMENT         => unaryOperator(PreIncrement)
      case DECREMENT         => unaryOperator(PreDecrement)
      case LBRACKET          =>
        eat(LBRACKET, RBRACKET)
        nextTokenKind match {
          case EQSIGN =>
            if (modifiers.contains(Static))
              ErrorStaticIndexingOperator("[]=", pos)
            val operatorType = ArrayAssign(Empty(), Empty(), Empty())
            eat(EQSIGN, LPAREN)
            val f1 = formal()
            eat(COMMA)
            val f2 = formal()
            (operatorType, List(f1, f2), modifiers)
          case LPAREN =>
            if (modifiers.contains(Static))
              ErrorStaticIndexingOperator("[]", pos)
            val operatorType = ArrayRead(Empty(), Empty())
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
    val retType = optional(returnType, COLON)
    eat(EQSIGN)

    OperatorDecl(operatorType, retType, args, statement(), newModifiers)
  }

  /**
   * <modifiers> ::= (pub | priv [ protected ] ) [ static ]
   */
  def modifiers(priv: TokenKind, pub: TokenKind): Set[Modifier] = {
    val access = accessRights(priv, pub)
    val modifiers: Set[Modifier] = Set(access)
    nextTokenKind match {
      case STATIC =>
        eat(STATIC)
        modifiers + Static
      case _      => modifiers
    }
  }

  /**
   * <tpe> ::= ( Int | Long | Float | Double | Bool | Char | String | <classIdentifier> ) { "[]" }
   */
  def tpe(): TypeTree = {
    val pos = nextToken
    val tpe = nextTokenKind match {
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
    var dimension = 0
    while (nextTokenKind == LBRACKET) {
      e.setPos(pos)
      eat(LBRACKET, RBRACKET)
      e = ArrayType(e)
      dimension += 1
    }
    if (dimension > ASTBuilder.MaximumArraySize)
      ErrorInvalidArrayDimension(dimension, pos)
    e.setPos(pos)
  }

  /**
   * <statement> ::= "{" { <statement> } "}
   * | <varDeclaration>
   * | if"(" <expression> ")" <statement> [ else <statement> ]
   * | while"(" <expression> ")" <statement>
   * | <forloop> <endStatement>
   * | println"(" <expression> ")" <endStatement>
   * | return [ <expression> ] <endStatement>
   * | <identifier> "=" <expression> <endStatement>
   * | <identifier> "+=" <expression> <endStatement>
   * | <identifier> "-=" <expression> <endStatement>
   * | <identifier> "*=" <expression> <endStatement>
   * | <identifier> "/=" <expression> <endStatement>
   * | <identifier> "%=" <expression> <endStatement>
   * | <identifier> "&=" <expression> <endStatement>
   * | <identifier> "|=" <expression> <endStatement>
   * | <identifier> "^=" <expression> <endStatement>
   * | <identifier> "<<=" <expression> <endStatement>
   * | <identifier> ">>=" <expression> <endStatement>
   * | <identifier> "[" <expression> "]" "=" <expression> <endStatement>
   * | <identifier> "++" <endStatement>
   * | <identifier> "--" <endStatement>
   * | "++" <identifier> <endStatement>
   * | "--" <identifier> <endStatement>
   * | <expression>"."<identifier>"(" [ <expression> { "," <expression> } ] [ "."<identifier>"(" [ <expression> { "," <expression> } ] } <endStatement>
   */
  def statement(): StatTree = {
    val pos = nextToken
    val tree = nextTokenKind match {
      case PRIVVAR =>
        val variable = localVarDeclaration()
        endStatement()
        variable
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
        forLoop()
      case PRINT   =>
        eat(PRINT, LPAREN)
        val expr = expression()
        eat(RPAREN)
        endStatement()
        Print(expr)
      case PRINTLN =>
        eat(PRINTLN, LPAREN)
        val expr = expression()
        eat(RPAREN)
        endStatement()
        Println(expr)
      case ERROR   =>
        eat(ERROR, LPAREN)
        val expr = expression()
        eat(RPAREN)
        endStatement()
        Error(expr)
      case RETURN  =>
        eat(RETURN)
        val expr = if (currentToken.kind != SEMICOLON && currentToken.kind != NEWLINE) Some(expression()) else None
        endStatement()
        Return(expr)
      case _       =>
        val expr = expression()
        endStatement()
        expr match {
          case stat: StatTree => stat
          case _              => FatalInvalidStatement(expr)
        }
    }
    tree.setPos(pos)
  }

  /**
   * <forloop> ::= for "(" <forInit> ";" [ <expression> ] ";" <forIncrement> ")" <statement>
   */
  def forLoop(): For = {
    eat(FOR, LPAREN)
    val init = forInit()
    eat(SEMICOLON)
    val condition = nextTokenKind match {
      case SEMICOLON => True() // if condition is left out, use 'true'
      case _         => expression()
    }
    eat(SEMICOLON)
    val post = forIncrement()
    eat(RPAREN)
    For(init, condition, post, statement())
  }

  /**
   * <forInit> ::= [ ( <assignment> | <varDeclaration> )  { "," ( <assignment> | <varDeclaration> ) }
   */
  def forInit(): List[StatTree] = {
    commaList(() => {
      nextTokenKind match {
        case PRIVVAR =>
          localVarDeclaration()
        case _       =>
          val id = identifier()
          nextTokenKind match {
            case EQSIGN | PLUSEQ |
                 MINUSEQ | DIVEQ |
                 MODEQ | ANDEQ |
                 OREQ | XOREQ |
                 LEFTSHIFTEQ | RIGHTSHIFTEQ =>
            case _                          => FatalWrongToken(EQSIGN, PLUSEQ, MINUSEQ, MULEQ, DIVEQ, MODEQ, ANDEQ, OREQ, XOREQ, LEFTSHIFTEQ, RIGHTSHIFTEQ)
          }
          assignment(Some(id)).asInstanceOf[Assign]
      }
    }, SEMICOLON)
  }

  /**
   * <forIncrement> ::= [ <expression> { "," <expression> } ]
   */
  def forIncrement(): List[StatTree] =
    commaList(() => nextTokenKind match {
      case INCREMENT =>
        eat(INCREMENT)
        PreIncrement(identifier())
      case DECREMENT =>
        eat(DECREMENT)
        PreDecrement(identifier())
      case IDKIND    =>
        val id = identifier()
        nextTokenKind match {
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

  /**
   * <endStatement> ::= ( ; | \n ) { ; | \n }
   */
  def endStatement(): Unit = currentToken.kind match {
    case SEMICOLON | NEWLINE =>
      readToken()
      while (currentToken.kind == SEMICOLON || currentToken.kind == NEWLINE)
        readToken()
    case _                   => FatalWrongToken(SEMICOLON, NEWLINE)
  }

  /**
   * <expression> ::= <assignment>
   */
  def expression(): ExprTree = {
    val pos = nextToken
    assignment().setPos(pos)
  }


  /**
   * <assignment> ::= <ternary> [ ( = | += | -= | *= | /= | %= | &= | |= | ^= | <<= | >>= ) <expression> ]
   * | <ternary> [ "[" <expression> "] = " <expression> ]
   */
  def assignment(expr: Option[ExprTree] = None) = {
    val e = if (expr.isDefined) expr.get else ternary()

    def assignment(constructor: Option[(ExprTree, ExprTree) => ExprTree]) = {
      eat(nextTokenKind)

      def assignmentExpr(expr: ExprTree) = if (constructor.isDefined) constructor.get(expr, expression()) else expression()

      e match {
        case ArrayRead(arr, index) => ArrayAssign(arr, index, assignmentExpr(e))
        case FieldRead(obj, id)    => FieldAssign(obj, id, assignmentExpr(e))
        case id: Identifier        => Assign(id, assignmentExpr(id))
        case _                     => FatalExpectedIdAssignment(e)
      }
    }

    nextTokenKind match {
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
    val pos = nextToken
    if (nextTokenKind == QUESTIONMARK) {
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
    while (nextTokenKind == INSTANCEOF) {
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
   * <term> ::= <termFirst> { termRest }
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
     * | <classIdentifier>
     * | <identifier> ++
     * | <identifier> --
     * | <identifier> "(" <expression> { "," <expression> } ")"
     * | true
     * | false
     * | this
     * | new <tpe>"[" <expression> "] { "[" <expression> "]" }
     * | new <classIdentifier> "(" [ <expression> { "," <expression> } ")"
     */
    def termFirst() = {
      val pos = nextToken
      val tree = nextTokenKind match {
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
        case HASH          =>
          eat(HASH)
          Hash(term())
        case DECREMENT     =>
          eat(DECREMENT)
          PreDecrement(expression())
        case INCREMENT     =>
          eat(INCREMENT)
          PreIncrement(expression())
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
          nextTokenKind match {
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
              MethodCall(Empty(), id, exprs)
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

          def sizes(): List[ExprTree] = {
            val sizes = untilNot(() => {
              eat(LBRACKET)
              val size = expression()
              eat(RBRACKET)
              size
            }, LBRACKET)
            if (sizes.size > ASTBuilder.MaximumArraySize)
              ErrorInvalidArrayDimension(sizes.size, pos)
            sizes
          }


          def primitiveArray(construct: () => TypeTree) = {
            eat(nextTokenKind)
            NewArray(construct(), sizes())
          }
          nextTokenKind match {
            case INT     => primitiveArray(IntType)
            case LONG    => primitiveArray(LongType)
            case FLOAT   => primitiveArray(FloatType)
            case DOUBLE  => primitiveArray(DoubleType)
            case CHAR    => primitiveArray(CharType)
            case STRING  => primitiveArray(StringType)
            case BOOLEAN => primitiveArray(BooleanType)
            case _       =>
              val id = classIdentifier()
              nextTokenKind match {
                case LPAREN   =>
                  eat(LPAREN)
                  val args = commaList(expression)
                  eat(RPAREN)
                  New(id, args)
                case LBRACKET =>
                  NewArray(id, sizes())
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
     * | ++
     * | --
     */
    def termRest(lhs: ExprTree): ExprTree = {
      val pos = nextToken
      var e = lhs
      val tokens = List(DOT, LBRACKET, AS, INCREMENT, DECREMENT)

      while (tokens.contains(nextTokenKind)) {
        e = nextTokenKind match {
          case DOT       =>
            eat(DOT)
            if (nextTokenKind == LENGTH) {
              eat(LENGTH)
              ArrayLength(e)
            } else {
              val id = identifier()
              if (nextTokenKind == LPAREN) {
                eat(LPAREN)
                val exprs = commaList(expression)
                eat(RPAREN)
                MethodCall(e, id, exprs.toList)
              } else {
                FieldRead(e, id)
              }
            }
          case LBRACKET  =>
            eat(LBRACKET)
            val expr = expression()
            eat(RBRACKET)
            ArrayRead(e, expr)
          case AS        =>
            eat(AS)
            As(e, tpe())
          case INCREMENT =>
            eat(INCREMENT)
            PostIncrement(e)
          case DECREMENT =>
            eat(DECREMENT)
            PostDecrement(e)
          case _         => e
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
    while (kinds.contains(nextTokenKind)) {
      kinds.foreach { kind =>
        if (nextTokenKind == kind) {
          val pos = nextToken
          eat(kind)
          expr = ASTBuilder.tokenMap(kind)(expr, next()).setPos(pos)
        }
      }
    }
    expr
  }


  /** Store the current token, as read from the lexer. */

  private def readToken(): Unit = {

    currentIndex += 1
    if (currentIndex < tokens.length) {
      currentToken = tokens(currentIndex)
      // skips bad tokens
      while (currentToken.kind == BAD) {
        currentIndex += 1
        currentToken = tokens(currentIndex)
      }

    }
  }

  private def eatNewLines(): Unit =
    while (currentToken.kind == NEWLINE)
      readToken()

  /** ''Eats'' the expected token, or terminates with an error. */
  private def eat(kind: TokenKind*): Unit = {
    eatNewLines()
    for (k <- kind) {
      if (nextTokenKind == k) {
        readToken()
      } else {
        FatalWrongToken(k)
      }
    }
  }
  /**
   * Parses the correct access rights given the private token and
   * public token to use.
   */
  private def accessRights(priv: TokenKind, pub: TokenKind) =
    nextTokenKind match {
      case x if x == pub  =>
        eat(pub)
        Public
      case x if x == priv =>
        eat(priv)
        if (nextTokenKind == PROTECTED) {
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
    if (nextTokenKind == RIGHTSHIFT) {
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
  private def classTypeIdentifier(): ClassIdentifier = nextToken match {
    case id: ID =>
      eat(IDKIND)
      val tIds = nextTokenKind match {
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
    val pos = nextToken
    val ids = nonEmptyList(identifier, DOT)
    val tIds = nextTokenKind match {
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
  private def identifier(): Identifier = nextToken match {
    case id: ID =>
      eat(IDKIND)
      Identifier(id.value).setPos(id)
    case _      => FatalWrongToken(IDKIND)
  }

  /**
   * <stringLit> ::= sequence of arbitrary characters, except new lines and "
   */
  private def stringLit(): StringLit = nextToken match {
    case strlit: STRLIT =>
      eat(STRLITKIND)
      StringLit(strlit.value).setPos(strlit)
    case _              => FatalWrongToken(STRLITKIND)
  }

  /**
   * <intLit> ::= sequence of digits, with no leading zeros
   */
  private def intLit(): IntLit = nextToken match {
    case intlit: INTLIT =>
      eat(INTLITKIND)
      IntLit(intlit.value).setPos(intlit)
    case _              => FatalWrongToken(INTLITKIND)
  }

  /**
   * <longLit> ::= sequence of digits, with no leading zeros ending with an 'l'
   */
  private def longLit(): LongLit = nextToken match {
    case longLit: LONGLIT =>
      eat(LONGLITKIND)
      LongLit(longLit.value).setPos(longLit)
    case _                => FatalWrongToken(LONGLITKIND)
  }

  /**
   * <floatLit> ::= sequence of digits, optionally with a single '.' ending with an 'f'
   */
  private def floatLit(): FloatLit = nextToken match {
    case floatLit: FLOATLIT =>
      eat(FLOATLITKIND)
      FloatLit(floatLit.value).setPos(floatLit)
    case _                  => FatalWrongToken(FLOATLITKIND)
  }


  /**
   * <doubleLit> ::= sequence of digits, optionally with a single '.' ending with an 'f'
   */
  private def doubleLit(): DoubleLit = nextToken match {
    case doubleLit: DOUBLELIT =>
      eat(DOUBLELITKIND)
      DoubleLit(doubleLit.value).setPos(doubleLit)
    case _                    => FatalWrongToken(DOUBLELITKIND)
  }

  /**
   * <charLit> ::= sequence of digits, optionally with a single '.' ending with an 'f'
   */
  private def charLit(): CharLit = nextToken match {
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
    while (nextTokenKind == delimiter) {
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
    if (nextTokenKind == stopSign) {
      List()
    } else {
      val arrBuff = new ArrayBuffer[T]()
      arrBuff += parse()
      while (currentToken.kind == COMMA || currentToken.kind == NEWLINE) {
        readToken()
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
    if (nextTokenKind == kind) {
      eat(kind)
      Some(parse())
    } else {
      None
    }
  }

  /**
   * Continues parsing until on of the given token kinds are encountered.
   */
  private def until[T](parse: () => T, kinds: TokenKind*): List[T] = {
    val condition = () => !kinds.contains(nextTokenKind)
    _until(condition, parse)
  }

  /**
   * Continues parsing until a token different from the given tokens are encountered.
   */
  private def untilNot[T](parse: () => T, kinds: TokenKind*): List[T] = {
    val condition = () => kinds.contains(nextTokenKind)
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

  private def ErrorInvalidArrayDimension(size: Int, pos: Positioned) =
    error(s"Invalid array dimension: '$size', ${ASTBuilder.MaximumArraySize} is the maximum dimension of an array.", pos)

  private def FatalInvalidStatement(pos: Positioned) =
    fatal("Not a valid statement, expected println, if, while, assignment, a method call or incrementation/decrementation. ", pos)

  private def FatalExpectedIdAssignment(pos: Positioned) =
    fatal("Expected identifier on left side of assignment.", pos)

  private def FatalWrongToken(kind: TokenKind, more: TokenKind*): Nothing = FatalWrongToken((kind :: more.toList).mkString(" or "), currentToken.toString, currentToken)

  private def FatalWrongToken(expected: String, found: String, pos: Positioned): Nothing =
    fatal(s"Expected $expected, found: $found.", pos)

}

object ASTBuilder {

  private val MaximumArraySize = 255

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