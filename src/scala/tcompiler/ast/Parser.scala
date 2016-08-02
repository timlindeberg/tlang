package tcompiler
package ast

import tcompiler.analyzer.Types.TUnit
import tcompiler.ast.Trees.{OperatorTree, _}
import tcompiler.imports.{ClassSymbolLocator, ImportMap, TemplateImporter}
import tcompiler.lexer.Tokens._
import tcompiler.lexer._
import tcompiler.utils.Extensions._
import tcompiler.utils._

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

object Parser extends Pipeline[List[List[Token]], List[CompilationUnit]] {

  def run(ctx: Context)(tokenList: List[List[Token]]): List[CompilationUnit] =
    tokenList.map { tokens =>
      val astBuilder = new ASTBuilder(ctx, tokens.toArray)
      astBuilder.parseGoal()
    }

}

object ASTBuilder {

  val MaximumArraySize = 255
  val TLangObject      = Main.TLangObject.replaceAll("/", "::")
  val TLangString      = Main.TLangString.replaceAll("/", "::")

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
                                                                                                LESSTHANEQ -> LessThanEquals,
                                                                                                GREATERTHAN -> GreaterThan,
                                                                                                GREATERTHANEQ -> GreaterThanEquals,
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

class ASTBuilder(override var ctx: Context, tokens: Array[Token]) extends ParserErrors {

  import ASTBuilder._

  private var currentIndex        = 0
  private var currentToken: Token = tokens(currentIndex)

  private def nextToken =
    if (currentToken.kind == NEWLINE) tokens(currentIndex + 1) else currentToken

  private def nextTokenKind = nextToken.kind


  /**
    * <goal> ::= [ <packageDeclaration> ] { <importDeclarataion> } { (<classDeclaration> | <methodDeclaration> | <statement> } <EOF>
    */
  def parseGoal() = {
    val startPos = nextToken
    val pack = packageDeclaration()
    val imp = untilNot(importDeclaration, IMPORT)

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

    val importMap = new ImportMap(imp, ctx)
    CompilationUnit(pack, classes, importMap).setPos(startPos, nextToken)
  }

  private def createMainClass(code: List[Tree]) = {
    var classes = code collect { case x: ClassDecl => x }
    val methods = code collect { case x: MethodDecl => x }
    val stats = code collect { case x: StatTree => x }

    if (stats.nonEmpty || methods.nonEmpty) {
      val mainName = currentToken.file.getName.dropRight(Main.FileEnding.length)
      val mainClass = classes.find(_.id.name == mainName) match {
        case Some(c) => c
        case None    =>
          val pos = if (stats.nonEmpty) stats.head else methods.head
          val m = ClassDecl(ClassID(mainName), List(), List(), List(), isAbstract = false)
          m.setPos(pos, nextToken)
          classes ::= m
          m
      }

      mainClass.methods :::= methods

      if (stats.nonEmpty) {
        // TODO: Main args should be kool::lang::String
        val args = List(Formal(ArrayType(ClassID("java::lang::String", List())), VariableID("args")))
        val modifiers: Set[Modifier] = Set(Public(), Static())
        val mainMethod = MethodDecl(Some(UnitType()), MethodID("main"), args, Some(Block(stats)), modifiers).setPos(stats.head, nextToken)
        mainClass.methods ::= mainMethod
      }
    }
    classes
  }

  /**
    * <packageDeclaration> ::= package <identifier> { . <identifier> }
    */
  def packageDeclaration() = {
    nextTokenKind match {
      case PACKAGE =>
        eat(PACKAGE)
        val startPos = nextToken
        val adress = nonEmptyList(() => identifierName(), COLON, COLON)
        endStatement()
        Package(adress).setPos(startPos, nextToken)
      case _       => Package(Nil)
    }
  }

  /**
    * <importDeclaration> ::= import <identifier> { . ( <identifier> | * ) }
    */
  def importDeclaration(): Import = {
    val startPos = nextToken
    eat(IMPORT)
    val ids = new ArrayBuffer[String]()

    ids += identifierName()
    while (nextTokenKind == COLON) {
      eat(COLON, COLON)
      nextTokenKind match {
        case TIMES =>
          eat(TIMES)
          endStatement()
          return WildCardImport(ids.toList).setPos(startPos, nextToken)
        case _     => ids += identifierName()
      }
    }
    endStatement()
    RegularImport(ids.toList).setPos(startPos, nextToken)
  }

  /**
    * <classDeclaration> ::= (class|trait) <classTypeIdentifier> <parents>
    * "{" { <varDeclaration> } { <methodDeclaration> } "}"
    */
  def classDeclaration(): ClassDecl = {
    val startPos = nextToken
    val isTrait = nextTokenKind match {
      case CLASS =>
        eat(CLASS)
        false
      case TRAIT =>
        eat(TRAIT)
        true
      case _     => FatalWrongToken(currentToken, CLASS, TRAIT)
    }
    val id = classTypeIdentifier()
    val parents = parentsDeclaration(isTrait)
    eat(LBRACE)
    val vars = untilNot(() => {
      val v = fieldDeclaration()
      endStatement()
      v
    }, PUBVAR, PRIVVAR, PUBVAL, PRIVVAL)
    val methods = untilNot(() => methodDeclaration(id.name), PRIVDEF, PUBDEF)
    eat(RBRACE)
    ClassDecl(id, parents, vars, methods, isTrait).setPos(startPos, nextToken)
  }

  /**
    * <parentsDeclaration> ::= [ : <classIdentifier> { "," <classIdentifier> } ]
    */
  def parentsDeclaration(isTrait: Boolean): List[ClassID] = nextTokenKind match {
    case COLON =>
      eat(COLON)
      nonEmptyList(classIdentifier, COMMA)
    case _     => List()
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
      case _       => FatalWrongToken(nextToken, PRIVVAR, PRIVVAL)
    }
    varDeclEnd(modifiers, startPos)
  }

  /**
    * <varDeclEnd> ::= <identifier> [ ":" <tpe> ] [ "=" <expression> ]
    */
  def varDeclEnd(modifiers: Set[Modifier], startPos: Positioned): VarDecl = {
    val id = varIdentifier()
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
    val id = varIdentifier()
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

    val id = methodIdentifier()
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
    val pos = nextToken
    eat(NEW)
    val methId = MethodID("new").setPos(pos, nextToken)
    eat(LPAREN)
    val args = commaList(formal)
    eat(RPAREN)
    val retType = Some(UnitType().setType(TUnit))
    val methBody = methodBody()
    ConstructorDecl(retType, methId, args, methBody, modifiers).setPos(startPos, nextToken)
  }

  /**
    * <returnType> ::= Unit | <tpe>
    */
  def returnType(): TypeTree = {
    val t = tpe()
    t match {
      case ClassID("Unit", _) => UnitType().setPos(t)
      case _                  => t
    }
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
    // arguments they should have. This is done since minus can have both
    // one or two operands. */

    def binaryOperator(constructor: (ExprTree, ExprTree) => OperatorTree): (OperatorTree, List[Formal], Set[Modifier]) = {
      eat(nextTokenKind)
      val operatorType = constructor(Empty(), Empty()).setType(TUnit)
      eat(LPAREN)
      val f1 = formal()
      eat(COMMA)
      val f2 = formal()
      (operatorType, List(f1, f2), modifiers + Static())
    }

    def unaryOperator(constructor: (ExprTree) => OperatorTree): (OperatorTree, List[Formal], Set[Modifier]) = {
      eat(nextTokenKind)
      val operatorType: OperatorTree = constructor(Empty()).setType(TUnit)
      eat(LPAREN)
      (operatorType, List(formal()), modifiers + Static())
    }

    val (operatorType, args, newModifiers) = nextTokenKind match {
      case PLUS          => binaryOperator(Plus)
      case MINUS         =>
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
      case TIMES         => binaryOperator(Times)
      case DIV           => binaryOperator(Div)
      case MODULO        => binaryOperator(Modulo)
      case LOGICAND      => binaryOperator(LogicAnd)
      case LOGICOR       => binaryOperator(LogicOr)
      case LOGICXOR      => binaryOperator(LogicXor)
      case LEFTSHIFT     => binaryOperator(LeftShift)
      case RIGHTSHIFT    => binaryOperator(RightShift)
      case LESSTHAN      => binaryOperator(LessThan)
      case LESSTHANEQ    => binaryOperator(LessThanEquals)
      case GREATERTHAN   => binaryOperator(GreaterThan)
      case GREATERTHANEQ => binaryOperator(GreaterThanEquals)
      case EQUALS        => binaryOperator(Equals)
      case NOTEQUALS     => binaryOperator(NotEquals)
      case LOGICNOT      => unaryOperator(LogicNot)
      case BANG          => unaryOperator(Not)
      case HASH          => unaryOperator(Hash)
      case INCREMENT     => unaryOperator(PreIncrement)
      case DECREMENT     => unaryOperator(PreDecrement)
      case LBRACKET      =>
        eat(LBRACKET, RBRACKET)
        nextTokenKind match {
          case EQSIGN =>
            modifiers.find(_.isInstanceOf[Static]) match {
              case Some(static) => ErrorStaticIndexingOperator("[]=", static)
              case None         =>
            }

            val operatorType: OperatorTree = Assign(ArrayRead(Empty(), Empty()), Empty())
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

            val operatorType: OperatorTree = ArrayRead(Empty(), Empty())
            eat(LPAREN)
            val f1 = formal()
            (operatorType, List(f1), modifiers)
          case _      => FatalWrongToken(nextToken, EQSIGN, LPAREN)
        }
      case _             =>
        FatalWrongToken(nextToken, PLUS, MINUS, TIMES, DIV, MODULO, LOGICAND, LOGICOR, LOGICXOR, LEFTSHIFT,
                         RIGHTSHIFT, LESSTHAN, LESSTHANEQ, GREATERTHAN, GREATERTHANEQ, EQUALS, NOTEQUALS, INCREMENT, DECREMENT,
                         LOGICNOT, BANG, LBRACKET)
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
      case _       => FatalWrongToken(nextToken, PUBDEF, PRIVDEF)
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
      case _       => FatalWrongToken(nextToken, PUBVAR, PRIVVAR, PUBVAL, PRIVVAL)
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
    * <basicTpe> ::= ( Int | Long | Float | Double | Bool | Char | String | <classIdentifier> )
    */
  def basicTpe(): TypeTree = {
    val startPos = nextToken
    val id = classIdentifier()

    // These are kind of keywords but are parsed by the lexer.
    // This enables names like java::lang::Long to be valid.
    val tpe = id.name match {
      case "Int"    => IntType()
      case "Long"   => LongType()
      case "Float"  => FloatType()
      case "Double" => DoubleType()
      case "Bool"   => BooleanType()
      case "Char"   => CharType()
      case _        => id
    }
    tpe.setPos(startPos, nextToken)
  }

  /**
    * <tpe> ::= <basicTpe> { "[]" | "?" }
    */
  def tpe(): TypeTree = {
    val startPos = nextToken
    var e = basicTpe()
    var dimension = 0

    while (nextTokenKind == QUESTIONMARK || nextTokenKind == LBRACKET) {
      e = nextTokenKind match {
        case QUESTIONMARK =>
          eat(QUESTIONMARK)
          NullableType(e).setPos(startPos, nextToken)
        case LBRACKET     =>
          eat(LBRACKET, RBRACKET)
          dimension += 1
          ArrayType(e).setPos(startPos, nextToken)
        case _            => ???
      }
    }
    if (dimension > MaximumArraySize)
      ErrorInvalidArrayDimension(dimension, e)

    e.setPos(startPos, nextToken)
  }

  /**
    * <statement> ::= "{" { <statement> } "}
    * | <varDeclaration>
    * | if"(" <expression> ")" <statement> [ else <statement> ]
    * | while"(" <expression> ")" <statement>
    * | <forloop> <endStatement>
    * | (print|println|error)"(" [ <expression> ] ")" <endStatement>
    * | break
    * | continue
    * | return [ <expression> ] <endStatement>
    * | <expression> <endStatement>
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
      case LBRACE                  =>
        eat(LBRACE)
        val stmts = until(statement, RBRACE)
        eat(RBRACE)
        Block(stmts)
      case IF                      =>
        eat(IF, LPAREN)
        val condition = expression()
        eat(RPAREN)
        val stmt = statement()
        val els = optional(statement, ELSE)
        If(condition, stmt, els)
      case WHILE                   =>
        eat(WHILE, LPAREN)
        val condition = expression()
        eat(RPAREN)
        While(condition, statement())
      case FOR                     =>
        forLoop()
      case PRINT | PRINTLN | ERROR =>
        val t = nextTokenKind
        eat(t)
        eat(LPAREN)
        val expr = nextTokenKind match {
          case RPAREN => StringLit("")
          case _      => expression()
        }
        eat(RPAREN)
        endStatement()
        t match {
          case PRINT   => Print(expr)
          case PRINTLN => Println(expr)
          case ERROR   => Error(expr)
          case _       => ???
        }
      case RETURN                  =>
        eat(RETURN)
        val expr = if (currentToken.kind != SEMICOLON && currentToken.kind != NEWLINE) Some(expression()) else None
        endStatement()
        Return(expr)
      case BREAK                   =>
        eat(BREAK)
        endStatement()
        Break()
      case CONTINUE                =>
        eat(CONTINUE)
        endStatement()
        Continue()
      case _                       =>
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
      case SEMICOLON => TrueLit() // if condition is left out, use 'true'
      case _         => expression()
    }
    eat(SEMICOLON)
    val post = commaList(expression)
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
          val id = varIdentifier()
          nextTokenKind match {
            case EQSIGN | PLUSEQ |
                 MINUSEQ | DIVEQ |
                 MODEQ | ANDEQ |
                 OREQ | XOREQ |
                 LEFTSHIFTEQ | RIGHTSHIFTEQ =>
              assignment(Some(id)).asInstanceOf[Assign].setPos(startPos, nextToken)
            case _                          => FatalWrongToken(nextToken, EQSIGN, PLUSEQ, MINUSEQ, MULEQ, DIVEQ, MODEQ, ANDEQ, OREQ, XOREQ, LEFTSHIFTEQ, RIGHTSHIFTEQ)
          }
      }
    }, SEMICOLON)

  /**
    * <endStatement> ::= ( ; | \n ) { ; | \n }
    */
  def endStatement(): Unit = currentToken.kind match {
    case SEMICOLON | NEWLINE =>
      readToken()
      while (currentToken.kind == SEMICOLON || currentToken.kind == NEWLINE)
        readToken()
    case EOF | RBRACE        =>
    case _                   => FatalWrongToken(nextToken, SEMICOLON, NEWLINE)
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
    val e = expr.getOrElse(ternary)

    def assignment(constructor: Option[(ExprTree, ExprTree) => ExprTree]) = {
      eat(nextTokenKind)

      def assignmentExpr(expr: ExprTree) = constructor match {
        case Some(cons) => cons(expr, expression())
        case None       => expression()
      }

      e match {
        case a: Assignable => Assign(a, assignmentExpr(e))
        case _             => FatalExpectedIdAssignment(e)
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


  /** <ternary> ::= <elvis> [ ? <elvis> : <elvis> ] */
  def ternary() = {
    val startPos = nextToken
    val e = elvis()
    if (nextTokenKind == QUESTIONMARK) {
      eat(QUESTIONMARK)
      val thn = elvis()
      eat(COLON)
      val els = elvis()
      Ternary(e, thn, els).setPos(startPos, nextToken)
    } else {
      e
    }
  }

  /** <elvis> ::= <or> [ ?: <or> ] */
  def elvis() = {
    val startPos = nextToken
    val e = or()
    if (nextTokenKind == ELVIS) {
      eat(ELVIS)
      val ifNull = or()
      Elvis(e, ifNull).setPos(startPos, nextToken)
    } else {
      e
    }
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

  /** <eqNotEq> ::= <is> { ( == | != ) <is> } */
  def eqNotEq() = leftAssociative(is, EQUALS, NOTEQUALS)

  /** <is> ::= <comparison> { inst <classIdentifier> } */
  def is() = {
    val startPos = nextToken
    var e = comparison()
    while (nextTokenKind == IS) {
      eat(IS)
      e = Is(e, tpe()).setPos(startPos, nextToken)
    }
    e
  }

  /** <comparison> ::= <bitShift> { ( < | <= | > | >= | inst ) <bitShift> } */
  def comparison() = leftAssociative(bitShift, LESSTHAN, LESSTHANEQ, GREATERTHAN, GREATERTHANEQ)

  /** <bitShift> ::= <plusMinus> { ( << | >> ) <plusMinus> } */
  def bitShift() = leftAssociative(plusMinus, LEFTSHIFT, RIGHTSHIFT)

  /** <plusMinus> ::= <timesDiv> { ( + | - ) <timesDiv> } */
  def plusMinus() = leftAssociative(timesDivMod, PLUS, MINUS)

  /** <timesDivMod> ::= <term> { ( * | / | % ) <term> } */
  def timesDivMod() = leftAssociative(term, TIMES, DIV, MODULO)

  /**
    * <term> ::= <termFirst> { termRest }
    */
  def term(): ExprTree = termRest(termFirst())


  /**
    * <termFirst> ::= "(" <expression> ")"
    * | "{" [ <expression> { "," <expression> } ] "}"
    * | ! <term>
    * | - <term>
    * | ~ <term>
    * | # <term>
    * | -- <term>
    * | ++ <term>
    * | <intLit>
    * | <longLit>
    * | <floatLit>
    * | <doubleLit>
    * | <charLit>
    * | <stringLit>
    * | <identifier> { :: <identifier> } [ "(" <expression> { "," <expression> } ")" ]
    * | <classIdentifier>
    * | "{" { <expression> } "}"
    * | true
    * | false
    * | this
    * | null
    * | <superCall>.<identifier>
    * | <superCall>.<identifier> "(" <expression> { "," <expression> } ")
    * | <newExpression>
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
      case MINUS         => negation()
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
        val ids = nonEmptyList(identifierName, COLON, COLON)
        val name = ids.mkString("::")
        val methStartPos = nextToken
        nextTokenKind match {
          case LPAREN =>
            eat(LPAREN)
            val exprs = commaList(expression)
            eat(RPAREN)
            val id = MethodID(name)
            val meth = MethodCall(id, exprs).setPos(methStartPos, nextToken)
            NormalAccess(Empty(), meth)
          case _      => VariableID(name)
        }
      case TRUE          =>
        eat(TRUE)
        TrueLit()
      case FALSE         =>
        eat(FALSE)
        FalseLit()
      case THIS          =>
        eat(THIS)
        This()
      case NULL          =>
        eat(NULL)
        NullLit()
      case SUPER         =>
        val sup = superCall()
        access(sup)
      case NEW           => newExpression()
      case _             => FatalUnexpectedToken(currentToken)
    }
    tree.setPos(startPos, nextToken)
  }

  /**
    * <termRest> ::= .<identifier>
    * | <access>
    * | <arrayIndexing>
    * | as <tpe>
    * | ++
    * | --
    * | !!
    */
  def termRest(lhs: ExprTree): ExprTree = {
    var e = lhs
    val tokens = List(DOT, SAFEACCESS, EXTRACTNULLABLE, LBRACKET, AS, INCREMENT, DECREMENT)

    // Uses current token since a newline should stop the iteration
    while (tokens.contains(currentToken.kind)) {
      val startPos = nextToken

      e = currentToken.kind match {
        case DOT | SAFEACCESS =>
          access(e)
        case EXTRACTNULLABLE  =>
          eat(EXTRACTNULLABLE)
          ExtractNullable(e)
        case LBRACKET         =>
          arrayIndexing(e)
        case AS               =>
          eat(AS)
          As(e, tpe())
        case INCREMENT        =>
          eat(INCREMENT)
          PostIncrement(e)
        case DECREMENT        =>
          eat(DECREMENT)
          PostDecrement(e)
        case _                => ???
      }
      e.setPos(startPos, nextToken)
    }

    e
  }

  /**
    * <access> ::= (. | ?.) <methodCall> | <identifier>
    */
  def access(obj: ExprTree) = {
    val startPos = nextToken
    val access = nextTokenKind match {
      case SAFEACCESS =>
        eat(SAFEACCESS)
        SafeAccess
      case DOT        =>
        eat(DOT)
        NormalAccess
      case _          => FatalWrongToken(nextToken, DOT, QUESTIONMARK)
    }

    val methStartPos = nextToken
    val id = varIdentifier()
    val application = nextTokenKind match {
      case LPAREN =>
        eat(LPAREN)
        val exprs = commaList(expression)
        eat(RPAREN)
        val methId = MethodID(id.name)
        MethodCall(methId, exprs.toList).setPos(methStartPos, nextToken)
      case _      => id
    }
    access(obj, application).setPos(startPos, nextToken)
  }

  /**
    * <negation> ::= - <term>
    */
  def negation(): ExprTree = {
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
  }

  /**
    * <newExpression> ::= new <basicTpe> [ "?[" <expression> "]" ] { "[" <expression> "]" [ "?" ] }
    */
  def newExpression(): ExprTree = {
    val startPos = nextToken
    eat(NEW)

    val tpe = basicTpe()

    val sizes = ListBuffer[ExprTree]()
    nextTokenKind match {
      case LPAREN                  =>
        eat(LPAREN)
        val args = commaList(expression)
        eat(RPAREN)
        New(tpe, args)
      case QUESTIONMARK | LBRACKET =>
        var e = tpe

        if (nextTokenKind == QUESTIONMARK) {
          eat(QUESTIONMARK, LBRACKET)
          val size = expression()
          eat(RBRACKET)
          sizes += size
          e = ArrayType(e).setPos(startPos, nextToken)
        }

        while (nextTokenKind == QUESTIONMARK || nextTokenKind == LBRACKET) {
          e = nextTokenKind match {
            case QUESTIONMARK =>
              eat(QUESTIONMARK)
              NullableType(e).setPos(startPos, nextToken)
            case LBRACKET     =>
              eat(LBRACKET)
              val size = expression()
              eat(RBRACKET)
              sizes += size
              ArrayType(e).setPos(startPos, nextToken)
          }
        }

        NewArray(e, sizes.toList)
      case _                       => FatalWrongToken(nextToken, LPAREN, LBRACKET)
    }
  }

  /**
    * <arrayIndexing> ::= "[" <expression> "]"
    * | "[" [ <expression> ] : [ <expression> ] "]"
    */
  def arrayIndexing(e: ExprTree): ExprTree = {
    eat(LBRACKET)
    nextTokenKind match {
      case COLON =>
        eat(COLON)
        nextTokenKind match {
          case RBRACKET =>
            eat(RBRACKET)
            ArraySlice(e, None, None)
          case _        =>
            val expr = expression()
            eat(RBRACKET)
            ArraySlice(e, None, Some(expr))
        }
      case _     =>
        val expr = expression()
        nextTokenKind match {
          case COLON    =>
            eat(COLON)
            nextTokenKind match {
              case RBRACKET =>
                eat(RBRACKET)
                ArraySlice(e, Some(expr), None)
              case _        =>
                val end = expression()
                eat(RBRACKET)
                ArraySlice(e, Some(expr), Some(end))
            }
          case RBRACKET =>
            eat(RBRACKET)
            ArrayRead(e, expr)
          case _        => FatalWrongToken(nextToken, COLON, RBRACKET)
        }
    }
  }

  /**
    * <superCall> ::= super [ "<" <classIdentifier> "> ]
    */
  def superCall(): ExprTree = {
    val startPos = nextToken
    eat(SUPER)
    val specifier = optional(() => {
      val id = classIdentifier()
      eat(GREATERTHAN)
      id
    }, LESSTHAN)
    Super(specifier).setPos(startPos, nextToken)
  }

  private def replaceExprWithReturnStat(stat: StatTree): StatTree = stat match {
    case Block(stmts) if stmts.nonEmpty =>
      val replaced = replaceExprWithReturnStat(stmts.last)
      Block(stmts.updated(stmts.size - 1, replaced))
    case acc@Access(_, m: MethodCall)   => Return(Some(acc))
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
      kinds foreach { kind =>
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
    if (currentIndex < tokens.length)
      currentToken = tokens(currentIndex)
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
        FatalWrongToken(nextToken, k)
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
      if (usedOneGreaterThan)
        eat(RIGHTSHIFT)

      usedOneGreaterThan = !usedOneGreaterThan
    } else {
      eat(GREATERTHAN)
    }

  /**
    * <classTypeIdentifier> ::= <identifier> [ "[" <identifier> { "," <identifier> } "]" ]
    */
  private def classTypeIdentifier(): ClassID = nextToken match {
    case id: ID =>
      eat(IDKIND)
      val tIds = nextTokenKind match {
        case LESSTHAN =>
          eat(LESSTHAN)
          val tmp = commaList(varIdentifier)
          eatRightShiftOrGreaterThan()
          tmp.map(x => ClassID(x.name, List()).setPos(x))
        case _        => List()
      }
      ClassID(id.value, tIds).setPos(id)
    case _      => FatalWrongToken(nextToken, IDKIND)
  }

  /**
    * <classIdentifier> ::= <identifier> { :: <identifier> } <templateList>
    */
  private def classIdentifier(): ClassID = {
    val startPos = nextToken
    val ids = nonEmptyList(identifierName, COLON, COLON)
    val id = ids.mkString("::")
    ClassID(id, templateList()).setPos(startPos, nextToken)
  }

  /**
    * <templateList> ::= [ "<" <type> { "," <type> } ">" ]
    */
  private def templateList(): List[TypeTree] = {
    nextTokenKind match {
      case LESSTHAN =>
        eat(LESSTHAN)
        val tmp = commaList(tpe)
        eatRightShiftOrGreaterThan()
        tmp
      case _        => List()
    }
  }

  /**
    * Fetches the name of the identifer without creating an identifier object.
    */
  private def identifierName(): String = nextToken match {
    case id: ID =>
      eat(IDKIND)
      id.value
    case _      => FatalWrongToken(nextToken, IDKIND)
  }

  /**
    * <varIdentifier> ::= sequence of letters, digits and underscores, starting with a letter and which is not a keyword
    */
  private def varIdentifier(): VariableID = nextToken match {
    case id: ID =>
      eat(IDKIND)
      VariableID(id.value).setPos(id, nextToken)
    case _      => FatalWrongToken(nextToken, IDKIND)
  }

  /**
    * <methodIdentifier> ::= sequence of letters, digits and underscores, starting with a letter and which is not a keyword
    */
  private def methodIdentifier(): MethodID = nextToken match {
    case id: ID =>
      eat(IDKIND)
      MethodID(id.value).setPos(id, nextToken)
    case _      => FatalWrongToken(nextToken, IDKIND)
  }

  /**
    * <stringLit> ::= sequence of arbitrary characters, except new lines and "
    */
  private def stringLit(): StringLit = nextToken match {
    case strlit: STRLIT =>
      eat(STRLITKIND)
      StringLit(strlit.value).setPos(strlit, nextToken)
    case _              => FatalWrongToken(nextToken, STRLITKIND)
  }

  /**
    * <intLit> ::= sequence of digits, with no leading zeros
    */
  private def intLit(): IntLit =
    nextToken match {
      case intLit: INTLIT =>
        eat(INTLITKIND)
        IntLit(intLit.value).setPos(intLit, nextToken)
      case _              => FatalWrongToken(nextToken, INTLITKIND)
    }

  /**
    * <longLit> ::= sequence of digits, with no leading zeros ending with an 'l'
    */
  private def longLit(): LongLit = nextToken match {
    case longLit: LONGLIT =>
      eat(LONGLITKIND)
      LongLit(longLit.value).setPos(longLit, nextToken)
    case _                => FatalWrongToken(nextToken, LONGLITKIND)
  }

  /**
    * <floatLit> ::= sequence of digits, optionally with a single '.' ending with an 'f'
    */
  private def floatLit(): FloatLit = nextToken match {
    case floatLit: FLOATLIT =>
      eat(FLOATLITKIND)
      FloatLit(floatLit.value).setPos(floatLit, nextToken)
    case _                  => FatalWrongToken(nextToken, FLOATLITKIND)
  }


  /**
    * <doubleLit> ::= sequence of digits, optionally with a single '.' ending with an 'f'
    */
  private def doubleLit(): DoubleLit = nextToken match {
    case doubleLit: DOUBLELIT =>
      eat(DOUBLELITKIND)
      DoubleLit(doubleLit.value).setPos(doubleLit, nextToken)
    case _                    => FatalWrongToken(nextToken, DOUBLELITKIND)
  }

  /**
    * <charLit> ::= sequence of digits, optionally with a single '.' ending with an 'f'
    */
  private def charLit(): CharLit = nextToken match {
    case charLit: CHARLIT =>
      eat(CHARLITKIND)
      CharLit(charLit.value).setPos(charLit, nextToken)
    case _                => FatalWrongToken(nextToken, CHARLITKIND)
  }

  /**
    * Parses lists of the form
    * <nonEmptyList> ::= <parse> { <delimiter_0> ... <delimiter_n> <parse> }
    */

  private def nonEmptyList[T](parse: () => T, delimiters: TokenKind*): List[T] = {
    val arrBuff = new ArrayBuffer[T]()
    arrBuff += parse()
    val first = delimiters.head
    while (nextTokenKind == first) {
      delimiters.foreach(eat(_))
      arrBuff += parse()
    }
    arrBuff.toList
  }

  /**
    * Parses a commalist of the form
    * <commaList> ::= [ <parse> { "," <parse> } ]
    */
  private def commaList[T](parse: () => T, stopSign: TokenKind = RPAREN): List[T] = {
    if (nextTokenKind == stopSign)
      return List()

    val arrBuff = new ArrayBuffer[T]()
    arrBuff += parse()
    while (currentToken.kind == COMMA || currentToken.kind == NEWLINE) {
      readToken()
      arrBuff += parse()
    }
    arrBuff.toList
  }

  /**
    * Parses an optional of the form
    * <optional> ::= [ parse ] and returns Option
    */
  private def optional[T](parse: () => T with Positioned, kinds: TokenKind*): Option[T] = {
    if (kinds.contains(nextTokenKind)) {
      eat(nextTokenKind)
      Some(parse())
    } else {
      None
    }
  }

  /**
    * Continues parsing until one of the given token kinds are encountered.
    */
  private def until[T](parse: () => T, kinds: TokenKind*): List[T] = {
    val condition = () => !kinds.contains(nextTokenKind)
    _until(condition, parse)
  }

  /**
    * Continues parsing until a token different from the given tokens is encountered.
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
}
