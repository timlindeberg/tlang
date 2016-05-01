package tcompiler
package ast

import java.util

import tcompiler.analyzer.Types.TUnit
import tcompiler.ast.TreeGroups.UselessStatement
import tcompiler.ast.Trees._
import tcompiler.lexer.Tokens._
import tcompiler.lexer._
import tcompiler.utils._

import scala.collection.mutable.ArrayBuffer

object Parser extends Pipeline[List[Token], Program] {

  def run(ctx: Context)(tokens: List[Token]): Program = {
    val astBuilder = new ASTBuilder(ctx, tokens.toArray)
    val lol = astBuilder.parseGoal()
    lol
  }

}

object ASTBuilder {

  private val LocationPrefix = "P"

  private val MaximumArraySize = 255

  private val tokenToUnaryOperatorAST: Map[TokenKind, ExprTree => ExprTree] = Map(
    LOGICNOT -> LogicNot,
    BANG -> Not,
    HASH -> Hash,
    INCREMENT -> PreIncrement,
    DECREMENT -> PreDecrement
  )


  private val tokenToBinaryOperatorAST: Map[TokenKind, (ExprTree, ExprTree) => ExprTree] = Map(
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

class ASTBuilder(ctx: Context, tokens: Array[Token]) {

  import ASTBuilder._

  private var currentIndex        = 0
  private var currentToken: Token = tokens(currentIndex)

  private def nextToken =
    if (currentToken.kind == NEWLINE) tokens(currentIndex + 1) else currentToken

  private def nextTokenKind = nextToken.kind


  /**
    * <goal> ::= [ <mainObject> ] { <classDeclaration> } <EOF>
    */
  def parseGoal() = {
    val startPos = nextToken
    val pack = optional(packageDecl, PACKAGE)
    val imp = untilNot(importDecl, IMPORT)

    val code = until(() => {
      nextTokenKind match {
        case CLASS | TRAIT    => classDeclaration()
        case PUBDEF | PRIVDEF =>
          val pos = nextToken
          val modifiers = methodModifiers()
          method(modifiers + Static(), pos)
        case _                => statement()
      }
    }, EOF)

    val classes = createMainClass(code)

    Program(pack, imp, classes).setPos(startPos, nextToken)
  }

  private def createMainClass(code: List[Tree]) = {
    var classes = code.collect { case x: ClassDecl => x }
    val methods = code.collect { case x: MethodDecl => x }
    val stats = code.collect { case x: StatTree => x }

    if (stats.nonEmpty || methods.nonEmpty) {
      val mainName = ctx.file.getName.dropRight(Main.FileEnding.length)
      val mainClass = classes.find(_.id.value == mainName) match {
        case Some(c) => c
        case None    =>
          val pos = if (stats.nonEmpty) stats.head else methods.head
          val m = InternalClassDecl(ClassIdentifier(mainName), List(), List(), List()).setPos(pos, nextToken)
          classes ::= m
          m
      }

      mainClass.methods :::= methods

      if (stats.nonEmpty) {
        val args = List(Formal(ArrayType(StringType()), Identifier("args")))
        val modifiers: Set[Modifier] = Set(Public(), Static())
        val mainMethod = MethodDecl(Some(UnitType()), Identifier("main"), args, Some(Block(stats)), modifiers).setPos(stats.head, nextToken)
        mainClass.methods ::= mainMethod
      }
    }
    classes
  }

  /**
    * <packageDecl> ::= package <identifier> { . <identifier> }
    */
  def packageDecl() = {
    val startPos = nextToken
    val identifiers = nonEmptyList(identifier, DOT)
    endStatement()
    Package(identifiers).setPos(startPos, nextToken)
  }

  /**
    * <importDecl> ::= import [ "<" ] <identifier> { . ( <identifier> | * ) } [ ">" ]
    */
  def importDecl(): Import = {
    val startPos = nextToken
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
      return GenericImport(ids.toList).setPos(startPos, nextToken)
    }
    ids += identifier()
    while (nextTokenKind == DOT) {
      eat(DOT)
      nextTokenKind match {
        case TIMES =>
          eat(TIMES)
          endStatement()
          return WildCardImport(ids.toList).setPos(startPos, nextToken)
        case _     => ids += identifier
      }
    }
    endStatement()
    RegularImport(ids.toList).setPos(startPos, nextToken)
  }

  /**
    * <classDeclaration> ::= (class|trait) <classIdentifier> <parents>
    * "{" { <varDeclaration> } { <methodDeclaration> } "}"
    */
  def classDeclaration(): ClassDecl = {
    val startPos = nextToken
    val classType = nextTokenKind match {
      case CLASS =>
        eat(CLASS)
        InternalClassDecl
      case TRAIT =>
        eat(TRAIT)
        Trait
      case _     => FatalWrongToken(CLASS, TRAIT)
    }
    val id = classTypeIdentifier()
    val parents = parentsDeclaration()
    eat(LBRACE)
    val vars = untilNot(() => {
      val v = fieldDeclaration()
      endStatement()
      v
    }, PUBVAR, PRIVVAR, PUBVAL, PRIVVAL)
    val methods = untilNot(() => methodDeclaration(id.value), PRIVDEF, PUBDEF)
    eat(RBRACE)
    classType(id, parents, vars, methods).setPos(startPos, nextToken)
  }

  /**
    * <parentsDeclaration> ::= [ : <classIdentifier> { "," <classIdentifier> } ]
    */
  def parentsDeclaration(): List[ClassIdentifier] = nextTokenKind match {
    case COLON =>
      eat(COLON)
      nonEmptyList(classIdentifier, COMMA)
    case _     =>
      List()
  }

  /**
    * <fieldDeclaration> ::= <fieldModifiers> <variableEnd>
    */
  def fieldDeclaration(): VarDecl = {
    val startPos = nextToken
    varDeclEnd(fieldModifiers(), startPos)
  }

  /**
    * <localVarDeclaration> ::= (var | val) <variableEnd>
    */
  def localVarDeclaration(): VarDecl = {
    val startPos = nextToken
    val modifiers: Set[Modifier] = nextTokenKind match {
      case PRIVVAR =>
        eat(PRIVVAR)
        Set(Private())
      case PRIVVAL =>
        eat(PRIVVAL)
        Set(Private(), Final())
      case _       => FatalWrongToken(PRIVVAR, PRIVVAL)
    }
    varDeclEnd(modifiers, startPos)
  }

  /**
    * <varDeclEnd> ::= <identifier> [ ":" <tpe> ] [ "=" <expression> ]
    */
  def varDeclEnd(modifiers: Set[Modifier], startPos: Positioned): VarDecl = {
    val id = identifier()
    val typ = optional(tpe, COLON)
    val endPos = nextToken
    val init = optional(expression, EQSIGN)
    VarDecl(typ, id, init, modifiers).setPos(startPos, endPos)
  }

  /**
    * <formal> ::= <identifier> ":" <tpe>
    */
  def formal(): Formal = {
    val startPos = nextToken
    val id = identifier()
    eat(COLON)
    val typ = tpe()
    Formal(typ, id).setPos(startPos, nextToken)
  }

  /**
    * <methodDeclaration> ::= <methodModifiers> ( <constructor> | <operator> | <method> )
    */
  def methodDeclaration(className: String): FuncTree = {

    val startPos = nextToken
    val mods = methodModifiers()
    nextTokenKind match {
      case IDKIND => method(mods, startPos)
      case NEW    => constructor(mods, className, startPos)
      case _      => operator(mods, startPos)
    }
  }

  /**
    * <method> ::= <identifier> "(" [ <formal> { "," <formal> } ] "): " (<tpe> | Unit) <methodBody>
    */
  def method(modifiers: Set[Modifier], startPos: Positioned): MethodDecl = {
    modifiers.find(_.isInstanceOf[Implicit]) match {
      case Some(impl) => ErrorImplicitMethodOrOperator(impl)
      case None       =>
    }

    val id = identifier()
    eat(LPAREN)
    val args = commaList(formal)
    eat(RPAREN)
    val retType = optional(returnType, COLON)
    val endPos = nextToken

    val methBody = methodBody()
    MethodDecl(retType, id, args, methBody, modifiers).setPos(startPos, endPos)
  }

  /**
    * [ "=" <statement> ]
    */
  def methodBody(): Option[StatTree] = optional(() => replaceExprWithReturnStat(statement()), EQSIGN)


  /**
    * <constructor> ::= new "(" [ <formal> { "," <formal> } ] ")"  "=" <statement>
    */
  def constructor(modifiers: Set[Modifier], className: String, startPos: Positioned): ConstructorDecl = {
    eat(NEW)
    eat(LPAREN)
    val args = commaList(formal)
    eat(RPAREN)
    val retType = Some(UnitType().setType(TUnit))
    val methBody = methodBody()
    ConstructorDecl(retType, Identifier("new"), args, methBody, modifiers).setPos(startPos, nextToken)
  }

  /**
    * <returnType> ::= Unit | <tpe>
    */
  def returnType(): TypeTree =
    if (nextTokenKind == UNIT) {
      val startPos = nextToken
      eat(UNIT)
      UnitType().setPos(startPos, nextToken)
    } else {
      tpe()
    }

  /**
    * <operator> ::= ( + | - | * | / | % | / | "|" | ^ | << | >> | < | <= | > | >= | ! | ~ | ++ | -- ) "(" <formal> [ "," <formal> ] "): <tpe>  = {" { <varDeclaration> } { <statement> } "}"
    **/
  def operator(modifiers: Set[Modifier], startPos: Positioned): OperatorDecl = {
    modifiers.find(_.isInstanceOf[Implicit]) match {
      case Some(impl) => ErrorImplicitMethodOrOperator(impl)
      case None       =>
    }

    // TODO: Find better way of parsing operators than hard coding how many
    // arguments they should have. This is done because since minus can have both
    // one or two operands. */

    def binaryOperator(constructor: (ExprTree, ExprTree) => ExprTree): (ExprTree, List[Formal], Set[Modifier]) = {
      eat(nextTokenKind)
      val operatorType = constructor(Empty(), Empty()).setType(TUnit)
      eat(LPAREN)
      val f1 = formal()
      eat(COMMA)
      val f2 = formal()
      (operatorType, List(f1, f2), modifiers + Static())
    }

    def unaryOperator(constructor: (ExprTree) => ExprTree): (ExprTree, List[Formal], Set[Modifier]) = {
      eat(nextTokenKind)
      val operatorType = constructor(Empty()).setType(TUnit)
      eat(LPAREN)
      (operatorType, List(formal()), modifiers + Static())
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
            val operatorType = Minus(Empty(), Empty()).setType(TUnit)
            (operatorType, List(f1, f2), modifiers + Static())
          case _     =>
            val operatorType = Negation(Empty()).setType(TUnit)
            (operatorType, List(f1), modifiers + Static())
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
            modifiers.find(_.isInstanceOf[Static]) match {
              case Some(static) => ErrorStaticIndexingOperator("[]=", static)
              case None         =>
            }

            val operatorType = ArrayAssign(Empty(), Empty(), Empty())
            eat(EQSIGN, LPAREN)
            val f1 = formal()
            eat(COMMA)
            val f2 = formal()
            (operatorType, List(f1, f2), modifiers)
          case LPAREN =>
            modifiers.find(_.isInstanceOf[Static]) match {
              case Some(static) => ErrorStaticIndexingOperator("[]", static)
              case None         =>
            }

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
    val endPos = nextToken
    val methBody = methodBody()

    OperatorDecl(operatorType, retType, args, methBody, newModifiers).setPos(startPos, endPos)
  }

  /**
    * <methodModifiers> ::= ( Def | def [ protected ])) [ static ] [ implicit ]
    */
  def methodModifiers(): Set[Modifier] = {
    val startPos = nextToken

    var modifiers: Set[Modifier] = nextTokenKind match {
      case PUBDEF  =>
        eat(PUBDEF)
        Set(Public().setPos(startPos, nextToken))
      case PRIVDEF =>
        eat(PRIVDEF)
        Set(protectedOrPrivate().setPos(startPos, nextToken))
      case _       => FatalWrongToken(PUBDEF, PRIVDEF)
    }

    while (nextTokenKind == STATIC || nextTokenKind == IMPLICIT) {
      val pos = nextToken
      val modifier = nextTokenKind match {
        case STATIC   =>
          eat(STATIC)
          Static().setPos(pos, nextToken)
        case IMPLICIT =>
          eat(IMPLICIT)
          Implicit().setPos(pos, nextToken)
        case _        => ???
      }
      modifiers += modifier
    }
    modifiers
  }

  private def protectedOrPrivate() = nextTokenKind match {
    case PROTECTED =>
      eat(PROTECTED)
      Protected()
    case _         => Private()
  }

  /**
    * <fieldModifiers> ::= ( Var | Val | (var | val [ protected ])) [ static ] [ implicit ]
    */
  def fieldModifiers(): Set[Modifier] = {
    val startPos = nextToken
    var modifiers: Set[Modifier] = nextTokenKind match {
      case PUBVAR  =>
        eat(PUBVAR)
        Set(Public().setPos(startPos, nextToken))
      case PRIVVAR =>
        eat(PRIVVAR)
        Set(protectedOrPrivate().setPos(startPos, nextToken))
      case PUBVAL  =>
        eat(PUBVAL)
        Set(Public().setPos(startPos, nextToken), Final().setPos(startPos, nextToken))
      case PRIVVAL =>
        eat(PRIVVAL)
        Set(protectedOrPrivate().setPos(startPos, nextToken), Final().setPos(startPos, nextToken))
      case _       => FatalWrongToken(PUBVAR, PRIVVAR, PUBVAL, PRIVVAL)
    }

    val pos = nextToken
    nextTokenKind match {
      case STATIC =>
        eat(STATIC)
        modifiers += Static().setPos(pos, nextToken)
      case _      =>
    }
    modifiers
  }

  /**
    * <tpe> ::= ( Int | Long | Float | Double | Bool | Char | String | <classIdentifier> ) { "[]" }
    */
  def tpe(): TypeTree = {
    val startPos = nextToken
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
    tpe.setPos(startPos, nextToken)
    var e = tpe
    var dimension = 0
    while (nextTokenKind == LBRACKET) {
      e.setPos(startPos, nextToken)
      eat(LBRACKET, RBRACKET)
      e = ArrayType(e)
      dimension += 1
    }
    e.setPos(startPos, nextToken)

    if (dimension > MaximumArraySize)
      ErrorInvalidArrayDimension(dimension, e)

    e
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
    **/
  def statement(): StatTree = {
    val startPos = nextToken

    // Variable needs custom end position in order to
    // only highlight expression up to equals sign
    nextTokenKind match {
      case PRIVVAR | PRIVVAL =>
        val variable = localVarDeclaration()
        endStatement()
        return variable
      case _                 =>
    }

    val tree = nextTokenKind match {
      case LBRACE   =>
        eat(LBRACE)
        val stmts = until(statement, RBRACE)
        eat(RBRACE)
        Block(stmts)
      case IF       =>
        eat(IF, LPAREN)
        val expr = expression()
        eat(RPAREN)
        val stmt = statement()
        val els = optional(statement, ELSE)
        If(expr, stmt, els)
      case WHILE    =>
        eat(WHILE, LPAREN)
        val expr = expression()
        eat(RPAREN)
        While(expr, statement())
      case FOR      =>
        forLoop()
      case PRINT    =>
        eat(PRINT, LPAREN)
        val expr = expression()
        eat(RPAREN)
        endStatement()
        Print(expr)
      case PRINTLN  =>
        eat(PRINTLN, LPAREN)
        val expr = expression()
        eat(RPAREN)
        endStatement()
        Println(expr)
      case ERROR    =>
        eat(ERROR, LPAREN)
        val expr = expression()
        eat(RPAREN)
        endStatement()
        Error(expr)
      case RETURN   =>
        eat(RETURN)
        val expr = if (currentToken.kind != SEMICOLON && currentToken.kind != NEWLINE) Some(expression()) else None
        endStatement()
        Return(expr)
      case BREAK    =>
        eat(BREAK)
        endStatement()
        Break()
      case CONTINUE =>
        eat(CONTINUE)
        endStatement()
        Continue()
      case _        =>
        val expr = expression()
        endStatement()
        expr
    }
    tree.setPos(startPos, nextToken)
  }

  /**
    * <forloop> ::= for "(" <forInit> ";" [ <expression> ] ";" <forIncrement> ")" <statement>
    */
  def forLoop(): StatTree = {
    val startPos = nextToken
    eat(FOR, LPAREN)

    nextTokenKind match {
      case PRIVVAR | PRIVVAL =>
        val varDecl = localVarDeclaration()
        if (varDecl.init.isDefined) {
          if (nextTokenKind == COMMA)
            eat(COMMA)
          regularForLoop(Some(varDecl), startPos)
        } else {
          nextTokenKind match {
            case IN =>
              forEachLoop(varDecl, startPos)
            case _  =>
              if (nextTokenKind == COMMA)
                eat(COMMA)
              regularForLoop(Some(varDecl), startPos)
          }
        }
      case _                 =>
        regularForLoop(None, startPos)
    }
  }

  /**
    * <regularForLoop> ::= <forInit> ";" [ <expression> ] ";" <forIncrement> ")" <statement>
    */
  def regularForLoop(firstVarDecl: Option[VarDecl], startPos: Positioned) = {
    val init = forInit()
    eat(SEMICOLON)
    val condition = nextTokenKind match {
      case SEMICOLON => True() // if condition is left out, use 'true'
      case _         => expression()
    }
    eat(SEMICOLON)
    val post = forIncrement()
    eat(RPAREN)
    val vars = firstVarDecl match {
      case Some(v) => v :: init
      case None    => init
    }
    For(vars, condition, post, statement()).setPos(startPos, nextToken)
  }

  /**
    * <forEachLoop> ::= in <expression> ")" <statement>
    */
  def forEachLoop(varDecl: VarDecl, startPos: Positioned) = {
    eat(IN)
    val container = expression()
    eat(RPAREN)
    Foreach(varDecl, container, statement()).setPos(startPos, nextToken)
  }

  /**
    * <forInit> ::= [ ( <assignment> | <varDeclaration> )  { "," ( <assignment> | <varDeclaration> ) }
    */
  def forInit(): List[StatTree] =
    commaList(() => {
      val startPos = nextToken
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
          assignment(Some(id)).asInstanceOf[Assign].setPos(startPos, nextToken)
      }
    }, SEMICOLON)


  /**
    * <forIncrement> ::= [ <expression> { "," <expression> } ]
    */
  def forIncrement(): List[StatTree] =
    commaList(() => {
      val startPos = nextToken
      val expr = nextTokenKind match {
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
      }
      expr.setPos(startPos, nextToken)
    })

  /**
    * <endStatement> ::= ( ; | \n ) { ; | \n }
    */
  def endStatement(): Unit = currentToken.kind match {
    case SEMICOLON | NEWLINE =>
      readToken()
      while (currentToken.kind == SEMICOLON || currentToken.kind == NEWLINE)
        readToken()
    case EOF | RBRACE        =>
    case _                   => FatalWrongToken(SEMICOLON, NEWLINE)
  }

  /**
    * <expression> ::= <assignment>
    */
  def expression(): ExprTree = {
    val startPos = nextToken
    assignment().setPos(startPos, nextToken)
  }


  /**
    * <assignment> ::= <ternary> [ ( = | += | -= | *= | /= | %= | &= | |= | ^= | <<= | >>= ) <expression> ]
    * | <ternary> [ "[" <expression> "] = " <expression> ]
    **/
  def assignment(expr: Option[ExprTree] = None) = {
    val e = if (expr.isDefined) expr.get else ternary()

    def assignment(constructor: Option[(ExprTree, ExprTree) => ExprTree]) = {
      eat(nextTokenKind)

      def assignmentExpr(expr: ExprTree) = if (constructor.isDefined) constructor.get(expr, expression()) else expression()

      e match {
        case ArrayRead(arr, index) => ArrayAssign(arr, index, assignmentExpr(e))
        case FieldRead(obj, id)    => FieldAssign(obj, id, assignmentExpr(e))
        case id: Identifier        => Assign(id, assignmentExpr(id))
        case _                     =>
          FatalExpectedIdAssignment(e)
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
    val startPos = nextToken
    var e = or()
    if (nextTokenKind == QUESTIONMARK) {
      eat(QUESTIONMARK)
      val thn = or()
      eat(COLON)
      val els = or()
      e = Ternary(e, thn, els).setPos(startPos, nextToken)
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
    val startPos = nextToken
    var e = comparison()
    while (nextTokenKind == INSTANCEOF) {
      eat(INSTANCEOF)
      e = Instance(e, identifier()).setPos(startPos, nextToken)
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
      * | "{" { <expression> } "}"
      * | true
      * | false
      * | this
      * | <superCall>.<identifier>
      * | <superCall>.<identifier> "(" <expression> { "," <expression> } ")
      * | new <tpe>"[" <expression> "] { "[" <expression> "]" }
      * | new <classIdentifier> "(" [ <expression> { "," <expression> } ")"
      */
    def termFirst() = {
      val startPos = nextToken
      val tree = nextTokenKind match {
        case LPAREN        =>
          eat(LPAREN)
          val expr = expression()
          eat(RPAREN)
          expr
        case LBRACE        =>
          eat(LBRACE)
          val expressions = commaList(expression, RBRACE)
          eat(RBRACE)
          ArrayLit(expressions)
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
          PreDecrement(term())
        case INCREMENT     =>
          eat(INCREMENT)
          PreIncrement(term())
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
        case SUPER         =>
          val sup = superCall()
          eat(DOT)
          val id = identifier()

          if (nextTokenKind == LPAREN) {
            eat(LPAREN)
            val exprs = commaList(expression)
            eat(RPAREN)
            MethodCall(sup, id, exprs.toList)
          } else {
            FieldRead(sup, id)
          }
        case NEW           =>
          eat(NEW)

          def sizes(): List[ExprTree] = {
            val sizes = untilNot(() => {
              eat(LBRACKET)
              val size = expression()
              eat(RBRACKET)
              size
            }, LBRACKET)
            if (sizes.size > MaximumArraySize)
              ErrorInvalidArrayDimension(sizes.size, startPos)
            sizes
          }

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
            case CHAR    =>
              eat(CHAR)
              CharType()
            case STRING  =>
              eat(STRING)
              StringType()
            case BOOLEAN =>
              eat(BOOLEAN)
              BooleanType()
            case _       => classIdentifier()
          }

          nextTokenKind match {
            case LPAREN   =>
              eat(LPAREN)
              val args = commaList(expression)
              eat(RPAREN)
              New(tpe, args)
            case LBRACKET =>
              NewArray(tpe, sizes())
            case _        => FatalWrongToken(LPAREN, LBRACKET)
          }
        case _             => FatalUnexpectedToken()
      }
      tree.setPos(startPos, nextToken)
    }

    /**
      * <termRest> ::= .<identifier>
      * | .<identifier> "(" <expression> { "," <expression> } ")
      * | "[" <expression> "]"
      * | as <tpe>
      * | ++
      * | --
      */
    def termRest(lhs: ExprTree): ExprTree = {

      // Cant be any rest if token is newline
      if (currentToken.kind == NEWLINE)
        return lhs

      var e = lhs
      val tokens = List(DOT, LBRACKET, AS, INCREMENT, DECREMENT)

      while (tokens.contains(nextTokenKind)) {
        val startPos = nextToken

        e = nextTokenKind match {
          case DOT       =>
            eat(DOT)
            val id = identifier()
            if (nextTokenKind == LPAREN) {
              eat(LPAREN)
              val exprs = commaList(expression)
              eat(RPAREN)
              MethodCall(e, id, exprs.toList)
            } else {
              FieldRead(e, id)
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
          case _         => ???
        }
        e.setPos(startPos, nextToken)
      }

      e
    }
    termRest(termFirst())
  }

  /**
    * <superCall> ::= super [ "<" <identifier> "> ]
    */
  def superCall(): ExprTree = {
    val startPos = nextToken
    eat(SUPER)
    val specifier = optional(() => {
      val id = identifier()
      eat(GREATERTHAN)
      id
    }, LESSTHAN)
    Super(specifier).setPos(startPos, nextToken)
  }

  private def replaceExprWithReturnStat(stat: StatTree): StatTree = stat match {
    case Block(stmts) if stmts.nonEmpty =>
      stmts.last match {
        case m: MethodCall       => Block(stmts.updated(stmts.size - 1, Return(Some(m))))
        case UselessStatement(e) => Block(stmts.updated(stmts.size - 1, Return(Some(e))))
        case _                   => stat
      }
    case m: MethodCall                  => Return(Some(m))
    case UselessStatement(e)            => Return(Some(e))
    case _                              => stat
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
          val startPos = currentToken
          eat(kind)
          expr = tokenToBinaryOperatorAST(kind)(expr, next()).setPos(startPos, nextToken)
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
          tmp.map(x => ClassIdentifier(x.value, List()).setPos(x))
        case _        => List()
      }
      ClassIdentifier(id.value, tIds).setPos(id)
    case _      => FatalWrongToken(IDKIND)
  }

  /**
    * <classIdentifier> ::= <identifier> [ "<" <type> { "," <type> } ">" ]
    */
  private def classIdentifier(): ClassIdentifier = {
    val startPos = nextToken
    val ids = nonEmptyList(identifier, DOT)
    val tIds = nextTokenKind match {
      case LESSTHAN =>
        eat(LESSTHAN)
        val tmp = commaList(tpe)
        eatRightShiftOrGreaterThan()
        tmp
      case _        => List()
    }
    ClassIdentifier(ids.map(_.value).mkString("."), tIds).setPos(startPos, nextToken)
  }

  /**
    * <identifier> ::= sequence of letters, digits and underscores, starting with a letter and which is not a keyword
    */
  private def identifier(): Identifier = nextToken match {
    case id: ID =>
      eat(IDKIND)
      Identifier(id.value).setPos(id, nextToken)
    case _      => FatalWrongToken(IDKIND)
  }

  /**
    * <stringLit> ::= sequence of arbitrary characters, except new lines and "
    */
  private def stringLit(): StringLit = nextToken match {
    case strlit: STRLIT =>
      eat(STRLITKIND)
      StringLit(strlit.value).setPos(strlit, nextToken)
    case _              => FatalWrongToken(STRLITKIND)
  }

  /**
    * <intLit> ::= sequence of digits, with no leading zeros
    */
  private def intLit(): IntLit =
    nextToken match {
      case intLit: INTLIT =>
        eat(INTLITKIND)
        IntLit(intLit.value).setPos(intLit, nextToken)
      case _              => FatalWrongToken(INTLITKIND)
    }

  /**
    * <longLit> ::= sequence of digits, with no leading zeros ending with an 'l'
    */
  private def longLit(): LongLit = nextToken match {
    case longLit: LONGLIT =>
      eat(LONGLITKIND)
      LongLit(longLit.value).setPos(longLit, nextToken)
    case _                => FatalWrongToken(LONGLITKIND)
  }

  /**
    * <floatLit> ::= sequence of digits, optionally with a single '.' ending with an 'f'
    */
  private def floatLit(): FloatLit = nextToken match {
    case floatLit: FLOATLIT =>
      eat(FLOATLITKIND)
      FloatLit(floatLit.value).setPos(floatLit, nextToken)
    case _                  => FatalWrongToken(FLOATLITKIND)
  }


  /**
    * <doubleLit> ::= sequence of digits, optionally with a single '.' ending with an 'f'
    */
  private def doubleLit(): DoubleLit = nextToken match {
    case doubleLit: DOUBLELIT =>
      eat(DOUBLELITKIND)
      DoubleLit(doubleLit.value).setPos(doubleLit, nextToken)
    case _                    => FatalWrongToken(DOUBLELITKIND)
  }

  /**
    * <charLit> ::= sequence of digits, optionally with a single '.' ending with an 'f'
    */
  private def charLit(): CharLit = nextToken match {
    case charLit: CHARLIT =>
      eat(CHARLITKIND)
      CharLit(charLit.value).setPos(charLit, nextToken)
    case _                => FatalWrongToken(CHARLITKIND)
  }

  /**
    * Parses lists of the form
    * <nonEmptyList> ::= <parse> { <delimiter> <parse> }
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
    * <commaList> ::= [ <parse> { "," <parse> } ]
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
  private def optional[T](parse: () => T with Positioned, kind: TokenKind): Option[T] = {
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

  private def error(errorCode: Int, msg: String, pos: Positioned) =
    ctx.reporter.error(LocationPrefix, errorCode, msg, pos)

  private def fatal(errorCode: Int, msg: String, pos: Positioned) =
    ctx.reporter.fatal(LocationPrefix, errorCode, msg, pos)

  //---------------------------------------------------------------------------------------
  //  Error messages
  //---------------------------------------------------------------------------------------

  private def ErrorImplicitMethodOrOperator(pos: Positioned) =
    error(0, "Only constructors can be declared implicit.", pos)

  private def ErrorStaticIndexingOperator(name: String, pos: Positioned) =
    error(1, s"Indexing operator '$name' cannot be declared static!", pos)

  private def ErrorInvalidArrayDimension(size: Int, pos: Positioned) =
    error(2, s"Invalid array dimension: '$size', $MaximumArraySize is the maximum dimension of an array.", pos)

  //---------------------------------------------------------------------------------------
  //  Fatal messages
  //---------------------------------------------------------------------------------------

  private def FatalExpectedIdAssignment(pos: Positioned) =
    fatal(1, "Expected identifier on left side of assignment.", pos)

  private def FatalWrongToken(kind: TokenKind, more: TokenKind*): Nothing =
    FatalWrongToken((kind :: more.toList).map(k => s"'$k'").mkString(" or "), currentToken.toString, currentToken)

  private def FatalWrongToken(expected: String, found: String, pos: Positioned): Nothing =
    fatal(2, s"Expected $expected, found: '$found'.", pos)

  private def FatalUnexpectedToken() =
    fatal(3, s"Unexpected token: '$currentToken'", currentToken)

}
