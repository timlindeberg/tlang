package tlang.compiler
package ast

import tlang.Context
import tlang.compiler.analyzer.Types.TUnit
import tlang.compiler.ast.Trees._
import tlang.compiler.imports.Imports
import tlang.compiler.lexer.Tokens._
import tlang.compiler.lexer._
import tlang.formatting.Formatting
import tlang.messages.{ErrorStringContext, Reporter}
import tlang.utils.Extensions._
import tlang.utils.Positioned

import scala.collection.mutable.{ArrayBuffer, ListBuffer}

object Parsing extends CompilerPhase[List[Token], CompilationUnit] {

  def run(ctx: Context)(tokenList: List[List[Token]]): List[CompilationUnit] =
    tokenList.map { tokens =>
      val errorStringContext = ErrorStringContext(ctx.formatter)
      val astBuilder = Parser(ctx, errorStringContext, tokens.toArray)
      astBuilder.compilationUnit
    }

  override def description(formatting: Formatting): String =
    "Parses the tokens produced by the lexing phase and generates an AST."

  override def printDebugOutput(output: List[CompilationUnit], debugOutputFormatter: DebugOutputFormatter): Unit =
    debugOutputFormatter.printASTs(phaseName, output)

}

object Parser {

  val MaximumArraySize = 255

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

case class Parser(ctx: Context, override val errorStringContext: ErrorStringContext, var tokens: Array[Token]) extends ParsingErrors {

  override val reporter: Reporter = ctx.reporter

  import Parser._

  // Remove comments and adjacant new line tokens
  tokens = tokens.filter(!_.isInstanceOf[COMMENTLIT])
  tokens = tokens.zipWithIndex.filter {
    case (token, i) => !(token.kind == NEWLINE && tokens(i + 1).kind == NEWLINE)
  }.map(_._1)

  protected val lastToken    = tokens.last
  private   var currentIndex = 0
  private   var currentToken = tokens(currentIndex)

  private def lastVisibleToken: Token = {
    if (currentIndex == 0)
      return currentToken

    var i = currentIndex - 1
    while (i > 0 && (tokens(i).kind in List(NEWLINE, INDENT, DEDENT)))
      i -= 1

    tokens(i)
  }

  private def nextToken = if (currentToken.kind == NEWLINE) tokens(currentIndex + 1) else currentToken

  private def nextTokenKind = nextToken.kind


  /**
    * <goal> ::= [ <packageDeclaration> ] { <importDeclaration> } { (<classDeclaration> | <methodDeclaration> | <statement> } <EOF>
    */
  def compilationUnit: CompilationUnit = positioned {
    val pack = packageDeclaration
    val imp = untilNot(importDeclaration, IMPORT)

    val code = until({
      nextTokenKind match {
        case CLASS | TRAIT | EXTENSION => classDeclaration
        case PUBDEF | PRIVDEF          =>
          positioned {
            val modifiers = methodModifiers
            method(modifiers + Static())
          }
        case _                         => statement
      }
    }, EOF)

    val classes = createMainClass(code)

    val imports = Imports(ctx, errorStringContext, imp, pack, classes)
    CompilationUnit(pack, classes, imports)
  }

  private def createMainClass(code: List[Tree]) = {
    var classes = code collect { case x: ClassDeclTree => x }
    val methods = code collect { case x: MethodDeclTree => x }
    val stats = code collect { case x: StatTree => x }

    if (stats.nonEmpty || methods.nonEmpty) {
      val mainName = currentToken.source.map(_.mainName).getOrElse("MissingSource")
      val mainClass = classes.filterInstance[IDClassDeclTree].find(_.id.name == mainName) match {
        case Some(c) => c
        case None    =>
          val pos = if (stats.nonEmpty) stats.head else methods.head
          val mainClass = ClassDecl(ClassID(mainName), List(), List(), List()).setPos(pos, lastVisibleToken)
          classes ::= mainClass
          mainClass
      }

      mainClass.methods :::= methods

      if (stats.nonEmpty) {
        val args = List(Formal(ArrayType(ClassID("java::lang::String", List())), VariableID("args")))
        val modifiers: Set[Modifier] = Set(Public(), Static())
        val mainMethod = MethodDecl(MethodID("main").setNoPos(), modifiers, args, Some(UnitType()), Some(Block(stats))).setPos(stats.head, lastVisibleToken)
        mainClass.methods ::= mainMethod
      }
    }
    classes
  }

  /**
    * <packageDeclaration> ::= package <identifier> { . <identifier> }
    */
  def packageDeclaration: Package = positioned {
    nextTokenKind match {
      case PACKAGE =>
        eat(PACKAGE)
        val address = nonEmptyList(identifierName, COLON, COLON)
        endStatement()
        Package(address)
      case _       => Package(Nil)
    }
  }

  /**
    * <importDeclaration> ::= import <identifier> { :: ( <identifier> | * | extension  ) }
    */
  def importDeclaration: Import = positioned {
    eat(IMPORT)
    val address = new ListBuffer[String]()

    address += identifierName
    var imp: Option[Import] = None
    while (nextTokenKind == COLON) {
      eat(COLON, COLON)
      nextTokenKind match {
        case TIMES     =>
          eat(TIMES)
          imp = Some(WildCardImport(address.toList))
        case EXTENSION =>
          imp = Some(extensionImport(address.toList))
        case _         => address += identifierName
      }
    }
    endStatement()
    imp.getOrElse(RegularImport(address.toList))
  }

  /**
    * <extensionImport> ::= extension <identifier> { :: <identifier> }
    */
  def extensionImport(address: List[String]): Import = {
    eat(EXTENSION)
    val className = new ListBuffer[String]()
    className += identifierName
    while (nextTokenKind == COLON) {
      eat(COLON, COLON)
      className += identifierName
    }

    ExtensionImport(address, className.toList)
  }

  /**
    * <classDeclaration> ::=
    * | <classOrTraitDeclaration>
    * | <extensionDeclaration>
    */
  def classDeclaration: ClassDeclTree =
    if (nextTokenKind == EXTENSION)
      extensionDeclaration
    else
      classOrTraitDeclaration

  /**
    * <classOrTraitDeclaration> ::= (class|trait) <classTypeIdentifier> <parentsDeclaration>
    * ( "=" <indent> { <varDeclaration> } { <methodDeclaration> } <dedent> | <endStatement> )
    */
  def classOrTraitDeclaration: ClassDeclTree = positioned {
    val isClass = oneOf(CLASS, TRAIT)
    val id = classTypeIdentifier
    val parents = parentsDeclaration
    val (vars, methods) = if (nextTokenKind == EQSIGN) {
      eat(EQSIGN)
      eat(INDENT)
      val vars = untilNot({
        val v = fieldDeclaration
        endStatement()
        v
      }, PUBVAR, PRIVVAR, PUBVAL, PRIVVAL)
      val methods = untilNot(methodDeclaration, PRIVDEF, PUBDEF)
      eat(DEDENT)
      (vars, methods)
    } else {
      endStatement()
      (Nil, Nil)
    }

    if (isClass)
      ClassDecl(id, parents, vars, methods)
    else
      TraitDecl(id, parents, vars, methods)
  }

  /**
    * <extensionDeclaration> ::= extension <tpe> ("= <indent>" { <methodDeclaration> } "<dedent>" | <endStatement> )
    */
  def extensionDeclaration: ExtensionDecl = positioned {
    eat(EXTENSION)
    val id = tpe
    val methods = if (nextTokenKind == EQSIGN) {
      eat(EQSIGN)
      eat(INDENT)
      val methods = untilNot(methodDeclaration, PRIVDEF, PUBDEF)
      eat(DEDENT)
      methods
    } else {
      endStatement()
      Nil
    }
    ExtensionDecl(id, methods)
  }


  /**
    * <parentsDeclaration> ::= [ : <classIdentifier> { "," <classIdentifier> } ]
    */
  def parentsDeclaration: List[ClassID] = nextTokenKind match {
    case COLON =>
      eat(COLON)
      nonEmptyList(classIdentifier, COMMA)
    case _     => Nil
  }

  /**
    * <fieldDeclaration> ::= <fieldModifiers> <variableEnd>
    */
  def fieldDeclaration: VarDecl = positioned {
    varDeclEnd(fieldModifiers)
  }

  /**
    * <localVarDeclaration> ::= (var | val) <variableEnd>
    */
  def localVarDeclaration: VarDecl = positioned {
    val isVar = oneOf(PRIVVAR, PRIVVAL)
    val modifiers: Set[Modifier] = if (isVar) Set(Private()) else Set(Private(), Final())
    varDeclEnd(modifiers)
  }

  /**
    * <varDeclEnd> ::= <identifier> [ ":" <tpe> ] [ "=" <expression> ]
    */
  def varDeclEnd(modifiers: Set[Modifier]): VarDecl = {
    val id = varIdentifier
    val typ = optional(tpe, COLON)
    val init = optional(expression, EQSIGN)
    VarDecl(id, typ, init, modifiers)
  }

  /**
    * <formal> ::= <identifier> ":" <tpe>
    */
  def formal: Formal = positioned {
    val id = varIdentifier
    eat(COLON)
    val typ = tpe
    Formal(typ, id)
  }

  /**
    * <methodDeclaration> ::= <methodModifiers> ( <constructor> | <operator> | <method> )
    */
  def methodDeclaration: MethodDeclTree = positioned {
    val mods = methodModifiers
    nextTokenKind match {
      case IDKIND => method(mods)
      case NEW    => constructor(mods)
      case _      => operator(mods)
    }
  }


  /**
    * <methodModifiers> ::= ( Def | def [ protected ])) [ static ] [ implicit ]
    */
  def methodModifiers: Set[Modifier] = {
    val startPos = nextToken


    var modifiers: Set[Modifier] = nextTokenKind match {
      case PUBDEF  =>
        eat(PUBDEF)
        Set(Public().setPos(startPos, lastVisibleToken))
      case PRIVDEF =>
        eat(PRIVDEF)
        Set(protectedOrPrivate.setPos(startPos, lastVisibleToken))
      case _       => report(WrongToken(nextToken, lastToken, PUBDEF, PRIVDEF))
    }

    while (nextTokenKind == STATIC || nextTokenKind == IMPLICIT) {
      val pos = nextToken
      val modifier = nextTokenKind match {
        case STATIC   =>
          eat(STATIC)
          Static().setPos(pos, lastVisibleToken)
        case IMPLICIT =>
          eat(IMPLICIT)
          Implicit().setPos(pos, lastVisibleToken)
        case _        => ???
      }
      modifiers += modifier
    }
    modifiers
  }

  /**
    * <method> ::= <identifier> "(" [ <formal> { "," <formal> } ] "): " <returnType> <methodBody>
    */
  def method(modifiers: Set[Modifier]): MethodDecl = {
    modifiers.filterInstance[Implicit].foreach(mod => report(ImplicitMethodOrOperator(mod)))

    val id = methodIdentifier
    eat(LPAREN)
    val args = commaList(formal)
    eat(RPAREN)
    val retType = optional(returnType, COLON)

    val methBody = methodBody
    MethodDecl(id, modifiers, args, retType, methBody)
  }

  /**
    * <constructor> ::= new "(" [ <formal> { "," <formal> } ] ")" <methodBody>
    */
  def constructor(modifiers: Set[Modifier]): ConstructorDecl = {
    val pos = nextToken
    eat(NEW)
    val methId = MethodID("new").setPos(pos, lastVisibleToken)
    eat(LPAREN)
    val args = commaList(formal)
    eat(RPAREN)
    val retType = Some(UnitType().setType(TUnit).setNoPos())
    val methBody = methodBody
    ConstructorDecl(methId, modifiers, args, retType, methBody)
  }


  /**
    * <operator> ::= ( + | - | * | / | % | / | "|" | ^ | << | >> | < | <= | > | >= | ! | ~ | ++ | -- ) "(" <formal> [ "," <formal> ] "): <tpe> <methodBody>
    **/
  def operator(modifiers: Set[Modifier]): OperatorDecl = {
    modifiers.findInstance[Implicit].ifDefined { impl =>
      report(ImplicitMethodOrOperator(impl))
    }

    // TODO: Find better way of parsing operators than hard coding how many
    // arguments they should have. This is done since minus can have both
    // one or two operands.

    def binaryOperator(constructor: (ExprTree, ExprTree) => OperatorTree): (OperatorTree, List[Formal], Set[Modifier]) = {
      eat(nextTokenKind)
      val operatorType = constructor(Empty(), Empty()).setType(TUnit)
      eat(LPAREN)
      val f1 = formal
      eat(COMMA)
      val f2 = formal
      (operatorType, List(f1, f2), modifiers + Static())
    }

    def unaryOperator(constructor: (ExprTree) => OperatorTree): (OperatorTree, List[Formal], Set[Modifier]) = {
      eat(nextTokenKind)
      val operatorType: OperatorTree = constructor(Empty()).setType(TUnit)
      eat(LPAREN)
      (operatorType, List(formal), modifiers + Static())
    }

    val (operatorType, args, newModifiers) = nextTokenKind match {
      case PLUS          => binaryOperator(Plus)
      case MINUS         =>
        eat(MINUS)
        eat(LPAREN)
        val f1 = formal
        nextTokenKind match {
          case COMMA =>
            eat(COMMA)
            val f2 = formal
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
      case HASH          => unaryOperator(Hash)
      case INCREMENT     => unaryOperator(PreIncrement)
      case DECREMENT     => unaryOperator(PreDecrement)
      case LBRACKET      =>
        eat(LBRACKET)
        val (numArgs, operatorType) = nextTokenKind match {
          case RBRACKET =>
            eat(RBRACKET)
            nextTokenKind match {
              case LPAREN =>
                eat(LPAREN)
                (1, ArrayRead(Empty(), Empty()))
              case EQSIGN =>
                eat(EQSIGN, LPAREN)
                (2, Assign(ArrayRead(Empty(), Empty()), Empty()))
              case _      => report(WrongToken(nextToken, lastToken, EQSIGN, LPAREN))
            }
          case COLON    =>
            eat(COLON, COLON, RBRACKET, LPAREN)
            (3, ArraySlice(Empty(), None, None, None))
          case _        => report(WrongToken(nextToken, lastToken, RBRACKET, COLON))
        }
        modifiers.findInstance[Static].ifDefined { static =>
          report(StaticIndexingOperator(static))
        }

        val args = commaList(formal)
        if (args.size != numArgs)
          report(UnexpectedToken(currentToken, lastToken))

        (operatorType, args, modifiers)
      case _             =>
        report(WrongToken(nextToken, lastToken, PLUS, MINUS, TIMES, DIV, MODULO, LOGICAND, LOGICOR, LOGICXOR, LEFTSHIFT,
          RIGHTSHIFT, LESSTHAN, LESSTHANEQ, GREATERTHAN, GREATERTHANEQ, EQUALS, NOTEQUALS, INCREMENT, DECREMENT,
          LOGICNOT, LBRACKET))
    }
    eat(RPAREN)
    val retType = optional(returnType, COLON)
    val methBody = methodBody


    OperatorDecl(operatorType.setNoPos(), newModifiers, args, retType, methBody)
  }


  /**
    * [ "=" <statement> ]
    */
  def methodBody: Option[StatTree] = optional(replaceExprWithReturnStat(statement), EQSIGN)

  private def protectedOrPrivate: Accessability = positioned {
    nextTokenKind match {
      case PROTECTED =>
        eat(PROTECTED)
        Protected()
      case _         => Private()
    }
  }

  /**
    * <fieldModifiers> ::= ( Var | Val | (var | val [ protected ])) [ static ] [ implicit ]
    */
  def fieldModifiers: Set[Modifier] = {
    val startPos = nextToken
    var modifiers: Set[Modifier] = nextTokenKind match {
      case PUBVAR  =>
        eat(PUBVAR)
        Set(Public().setPos(startPos, lastVisibleToken))
      case PRIVVAR =>
        eat(PRIVVAR)
        Set(protectedOrPrivate.setPos(startPos, lastVisibleToken))
      case PUBVAL  =>
        eat(PUBVAL)
        Set(Public().setPos(startPos, lastVisibleToken), Final().setPos(startPos, lastVisibleToken))
      case PRIVVAL =>
        eat(PRIVVAL)
        Set(protectedOrPrivate.setPos(startPos, lastVisibleToken), Final().setPos(startPos, lastVisibleToken))
      case _       => report(WrongToken(nextToken, lastToken, PUBVAR, PRIVVAR, PUBVAL, PRIVVAL))
    }

    val pos = nextToken
    nextTokenKind match {
      case STATIC =>
        eat(STATIC)
        modifiers += Static().setPos(pos, lastVisibleToken)
      case _      =>
    }
    modifiers
  }

  /**
    * <tpe> ::= <classIdentifier> { "[]" | "?" }
    */
  def tpe: TypeTree = {
    val startPos = nextToken
    var e: TypeTree = classIdentifier
    var dimension = 0

    while (nextTokenKind == QUESTIONMARK || nextTokenKind == LBRACKET) {
      e = nextTokenKind match {
        case QUESTIONMARK =>
          eat(QUESTIONMARK)
          NullableType(e)
        case LBRACKET     =>
          eat(LBRACKET, RBRACKET)
          dimension += 1
          ArrayType(e)
        case _            => ???
      }
      e.setPos(startPos, lastVisibleToken)
    }
    if (dimension > MaximumArraySize)
      report(InvalidArrayDimension(dimension, e))

    e
  }

  /**
    * <returnType> ::= Unit | <tpe>
    */
  def returnType: TypeTree = positioned {
    val t = tpe
    t match {
      case ClassID("Unit", _) => UnitType()
      case _                  => t
    }
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
  def statement: StatTree = positioned {
    // Variable needs custom end position in order to
    // only highlight expression up to equals sign
    nextTokenKind match {
      case PRIVVAR | PRIVVAL =>
        val variable = localVarDeclaration
        endStatement()
        return variable
      case _                 =>
    }

    nextTokenKind match {
      case INDENT                  =>
        eat(INDENT)
        val stmts = until(statement, DEDENT)
        eat(DEDENT)
        Block(stmts)
      case IF                      =>
        eat(IF, LPAREN)
        val condition = expression
        eat(RPAREN)
        val stmt = statement
        val els = optional(statement, ELSE)
        If(condition, stmt, els)
      case WHILE                   =>
        eat(WHILE, LPAREN)
        val condition = expression
        eat(RPAREN)
        While(condition, statement)
      case FOR                     =>
        forLoop
      case PRINT | PRINTLN | ERROR =>
        val methType = nextTokenKind
        eat(methType)
        eat(LPAREN)
        val expr = nextTokenKind match {
          case RPAREN => StringLit("")
          case _      => expression
        }
        eat(RPAREN)
        endStatement()
        methType match {
          case PRINT   => Print(expr)
          case PRINTLN => Println(expr)
          case ERROR   => Error(expr)
          case _       => ???
        }
      case RETURN                  =>
        eat(RETURN)
        val expr = if (currentToken.kind != SEMICOLON && currentToken.kind != NEWLINE) Some(expression) else None
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
      case SEMICOLON               =>
        eat(SEMICOLON)
        Block(Nil)
      case _                       =>
        val expr = expression
        endStatement()
        expr
    }
  }

  /**
    * <forloop> ::= for "(" <forInit> ";" [ <expression> ] ";" <forIncrement> ")" <statement>
    */
  def forLoop: StatTree = positioned {
    eat(FOR, LPAREN)

    nextTokenKind match {
      case PRIVVAR | PRIVVAL =>
        val varDecl = localVarDeclaration
        if (varDecl.initiation.isDefined) {
          if (nextTokenKind == COMMA)
            eat(COMMA)
          regularForLoop(Some(varDecl))
        } else {
          nextTokenKind match {
            case IN =>
              forEachLoop(varDecl)
            case _  =>
              if (nextTokenKind == COMMA)
                eat(COMMA)
              regularForLoop(Some(varDecl))
          }
        }
      case _                 =>
        regularForLoop(None)
    }
  }

  /**
    * <regularForLoop> ::= <forInit> ";" [ <expression> ] ";" <forIncrement> ")" <statement>
    */
  def regularForLoop(firstVarDecl: Option[VarDecl]): For = {
    val init = forInit
    eat(SEMICOLON)
    val condition = nextTokenKind match {
      case SEMICOLON => TrueLit() // if condition is left out, use 'true'
      case _         => expression
    }
    eat(SEMICOLON)
    val post = commaList(expression)
    eat(RPAREN)
    val vars = firstVarDecl match {
      case Some(v) => v :: init
      case None    => init
    }
    For(vars, condition, post, statement)
  }

  /**
    * <forEachLoop> ::= in <expression> ")" <statement>
    */
  def forEachLoop(varDecl: VarDecl): Foreach = {
    eat(IN)
    val container = expression
    eat(RPAREN)
    Foreach(varDecl, container, statement)
  }

  /**
    * <forInit> ::= [ ( <assignment> | <varDeclaration> )  { "," ( <assignment> | <varDeclaration> ) }
    */
  def forInit: List[StatTree] =
    commaList({
      val startPos = nextToken
      nextTokenKind match {
        case PRIVVAR =>
          localVarDeclaration
        case _       =>
          val id = varIdentifier
          nextTokenKind match {
            case EQSIGN | PLUSEQ |
                 MINUSEQ | DIVEQ |
                 MODEQ | ANDEQ |
                 OREQ | XOREQ |
                 LEFTSHIFTEQ | RIGHTSHIFTEQ =>
              assignment(Some(id)).asInstanceOf[Assign].setPos(startPos, lastVisibleToken)
            case _                          =>
              report(WrongToken(nextToken, lastToken, EQSIGN, PLUSEQ, MINUSEQ, MULEQ, DIVEQ, MODEQ,
                ANDEQ, OREQ, XOREQ, LEFTSHIFTEQ, RIGHTSHIFTEQ))
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
    case _                   => report(WrongToken(nextToken, lastToken, SEMICOLON, NEWLINE))
  }

  /**
    * <expression> ::= <assignment>
    */
  def expression: ExprTree = assignment()

  /**
    * <assignment> ::= <ternary> [ ( = | += | -= | *= | /= | %= | &= | |= | ^= | <<= | >>= ) <expression> ]
    * | <ternary> [ "[" <expression> "] = " <expression> ]
    **/
  def assignment(expr: Option[ExprTree] = None): ExprTree = positioned {
    val startPos = nextToken
    val e = expr.getOrElse(ternary)

    def assignment(constructor: Option[(ExprTree, ExprTree) => ExprTree]) = {
      eat(nextTokenKind)

      val assignmentExpr = constructor
        .map(cons => cons(e, expression).setPos(startPos, lastVisibleToken))
        .getOrElse(expression)

      e match {
        case a: Assignable => Assign(a, assignmentExpr)
        case _             => report(ExpectedIdAssignment(e))
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
  def ternary: ExprTree = positioned {
    val e = elvis
    if (nextTokenKind == QUESTIONMARK) {
      eat(QUESTIONMARK)
      val thn = elvis
      eat(COLON)
      val els = elvis
      Ternary(e, thn, els)
    } else {
      e
    }
  }

  /** <elvis> ::= <or> [ ?: <or> ] */
  def elvis: ExprTree = positioned {
    val e = or
    if (nextTokenKind == ELVIS) {
      eat(ELVIS)
      val ifNull = or
      Elvis(e, ifNull)
    } else {
      e
    }
  }

  /** <or> ::= <and> { || <and> } */
  def or: ExprTree = leftAssociative(and, OR)

  /** <and> ::= <logicOr> { && <logicOr> } */
  def and: ExprTree = leftAssociative(logicOr, AND)

  /** <logicOr> ::= <logicXor> { | <logicXor> } */
  def logicOr: ExprTree = leftAssociative(logicXor, LOGICOR)

  /** <logicXor> ::= <logicAnd> { ^ <logicAnd> } */
  def logicXor: ExprTree = leftAssociative(logicAnd, LOGICXOR)

  /** <logicAnd> ::= <eqNotEq> { & <eqNotEq> } */
  def logicAnd: ExprTree = leftAssociative(eqNotEq, LOGICAND)

  /** <eqNotEq> ::= <is> { ( == | != ) <is> } */
  def eqNotEq: ExprTree = leftAssociative(is, EQUALS, NOTEQUALS)

  /** <is> ::= <comparison> { inst <classIdentifier> } */
  def is: ExprTree = positioned {
    var e = comparison
    while (nextTokenKind == IS) {
      eat(IS)
      e = Is(e, tpe)
    }
    e
  }

  /** <comparison> ::= <bitShift> { ( < | <= | > | >= | inst ) <bitShift> } */
  def comparison: ExprTree = leftAssociative(bitShift, LESSTHAN, LESSTHANEQ, GREATERTHAN, GREATERTHANEQ)

  /** <bitShift> ::= <plusMinus> { ( << | >> ) <plusMinus> } */
  def bitShift: ExprTree = leftAssociative(plusMinus, LEFTSHIFT, RIGHTSHIFT)

  /** <plusMinus> ::= <timesDiv> { ( + | - ) <timesDiv> } */
  def plusMinus: ExprTree = leftAssociative(timesDivMod, PLUS, MINUS)

  /** <timesDivMod> ::= <term> { ( * | / | % ) <term> } */
  def timesDivMod: ExprTree = leftAssociative(term, TIMES, DIV, MODULO)

  /**
    * <term> ::= <termFirst> { termRest }
    */
  def term: ExprTree = termRest(termFirst)

  /**
    * <termFirst> ::= "(" <expression> ")"
    * | "[" [ <expression> { "," <expression> } ] "]"
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
  def termFirst: ExprTree = positioned {
    nextTokenKind match {
      case LPAREN        =>
        eat(LPAREN)
        val expr = expression
        eat(RPAREN)
        expr
      case LBRACKET      =>
        eat(LBRACKET)
        val expressions = commaList(expression, RBRACKET)
        eat(RBRACKET)
        ArrayLit(expressions)
      case BANG          =>
        eat(BANG)
        Not(term)
      case MINUS         => negation
      case LOGICNOT      =>
        eat(LOGICNOT)
        LogicNot(term)
      case HASH          =>
        eat(HASH)
        Hash(term)
      case DECREMENT     =>
        eat(DECREMENT)
        PreDecrement(term)
      case INCREMENT     =>
        eat(INCREMENT)
        PreIncrement(term)
      case INTLITKIND    => intLit
      case LONGLITKIND   => longLit
      case FLOATLITKIND  => floatLit
      case DOUBLELITKIND => doubleLit
      case CHARLITKIND   => charLit
      case STRLITKIND    => stringLit
      case IDKIND        =>
        val methStartPos = nextToken
        val ids = nonEmptyList(identifierName, COLON, COLON)
        val name = ids.mkString("::")
        nextTokenKind match {
          case LPAREN =>
            val id = MethodID(name).setPos(methStartPos, lastVisibleToken)
            eat(LPAREN)
            val exprs = commaList(expression)
            eat(RPAREN)
            val meth = MethodCall(id, exprs).setPos(methStartPos, lastVisibleToken)
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
        val sup = superCall
        access(sup)
      case NEW           => newExpression
      case _             =>
        report(UnexpectedToken(currentToken, lastToken))
    }
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
  def termRest(termFirst: ExprTree): ExprTree = {
    var e = termFirst
    val tokens = List(DOT, SAFEACCESS, EXTRACTNULLABLE, LBRACKET, AS, INCREMENT, DECREMENT)

    // Uses current token since a newline should stop the iteration
    while (tokens.contains(currentToken.kind)) {

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
          As(e, tpe)
        case INCREMENT        =>
          eat(INCREMENT)
          PostIncrement(e)
        case DECREMENT        =>
          eat(DECREMENT)
          PostDecrement(e)
        case _                => ???
      }
      e.setPos(termFirst, lastVisibleToken)
    }

    e
  }

  /**
    * <access> ::= (. | ?.) <methodCall> | <identifier>
    */
  def access(obj: ExprTree): Access = positioned {
    val access = nextTokenKind match {
      case SAFEACCESS =>
        eat(SAFEACCESS)
        SafeAccess
      case DOT        =>
        eat(DOT)
        NormalAccess
      case _          => report(WrongToken(nextToken, lastToken, DOT, QUESTIONMARK))
    }

    val application = positioned {
      val id = varIdentifier
      nextTokenKind match {
        case LPAREN =>
          eat(LPAREN)
          val exprs = commaList(expression)
          eat(RPAREN)
          val methId = MethodID(id.name).setPos(id)
          MethodCall(methId, exprs)
        case _      => id
      }
    }

    access(obj, application)
  }

  /**
    * <negation> ::= - <term>
    */
  def negation: ExprTree = positioned {
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
        Negation(term)
    }
  }

  /**
    * <newExpression> ::= new <basicTpe> [ "?" <nullableBracket> ] { <nullableBracket }
    */
  def newExpression: ExprTree = {
    val startPos = nextToken
    eat(NEW)
    val tpe: TypeTree = classIdentifier

    nextTokenKind match {
      case LPAREN                  =>
        eat(LPAREN)
        val args = commaList(expression)
        eat(RPAREN)
        New(tpe, args)
      case QUESTIONMARK | LBRACKET =>
        var e = tpe
        val sizes = ListBuffer[ExprTree]()

        def _nullableBracket() = {
          val (t, size) = nullableBracket(startPos, e)
          e = t
          sizes += size
        }

        // If it starts with question mark a bracket must follow
        if (nextTokenKind == QUESTIONMARK) {
          eat(QUESTIONMARK)
          e = NullableType(e).setPos(startPos, lastVisibleToken)
          _nullableBracket()
        }

        while (nextTokenKind == LBRACKET)
          _nullableBracket()

        NewArray(e, sizes.toList)
      case _                       => report(WrongToken(nextToken, lastToken, LPAREN, LBRACKET))
    }
  }

  /**
    * <nullableBracket> ::=  "[" <expression> "]" [ "?" ]
    */
  def nullableBracket(startPos: Positioned, tpe: TypeTree): (TypeTree, ExprTree) = {
    var e = tpe
    eat(LBRACKET)
    val size = expression
    eat(RBRACKET)
    e = ArrayType(e).setPos(startPos, lastVisibleToken)
    if (nextTokenKind == QUESTIONMARK) {
      eat(QUESTIONMARK)
      e = NullableType(e).setPos(startPos, lastVisibleToken)
    }
    (e, size)
  }

  /**
    * <arrayIndexing> ::= "[" <expression> "]"
    * | "[" [ <expression> ] : [ <expression> ] "]"
    * | "[" [ <expression> ] : [ <expression> ] : [ <expression> ] "]"
    */
  def arrayIndexing(arr: ExprTree): ExprTree = positioned {
    eat(LBRACKET)
    val exprs = until({
      nextTokenKind match {
        case COLON =>
          eat(COLON)
          None
        case _     =>
          Some(expression)
      }
    }, RBRACKET)
    eat(RBRACKET)

    // @formatter:off
    exprs match {
      case Some(e) :: Nil                                          => ArrayRead(arr, e)                             // [e]
      case None :: Nil                                             => ArraySlice(arr, None, None, None)             // [:]
      case Some(e) :: None :: Nil                                  => ArraySlice(arr, Some(e), None, None)          // [e:]
      case None :: Some(e) :: Nil                                  => ArraySlice(arr, None, Some(e), None)          // [:e]
      case Some(e1) :: None :: Some(e2) :: Nil                     => ArraySlice(arr, Some(e1), Some(e2), None)     // [e:e]
      case None :: None :: Nil                                     => ArraySlice(arr, None, None, None)             // [::]
      case Some(e) :: None :: None :: Nil                          => ArraySlice(arr, Some(e), None, None)          // [e::]
      case None :: Some(e) :: None :: Nil                          => ArraySlice(arr, None, Some(e), None)          // [:e:]
      case None :: None :: Some(e) :: Nil                          => ArraySlice(arr, None, None, Some(e))          // [::e]
      case Some(e1) :: None :: Some(e2) :: None :: Nil             => ArraySlice(arr, Some(e1), Some(e2), None)     // [e:e:]
      case Some(e1) :: None :: None :: Some(e2) :: Nil             => ArraySlice(arr, Some(e1), None, Some(e2))     // [e::e]
      case None :: Some(e1) :: None :: Some(e2) :: Nil             => ArraySlice(arr, None, Some(e1), Some(e2))     // [:e:e]
      case Some(e1) :: None :: Some(e2) :: None :: Some(e3) :: Nil => ArraySlice(arr, Some(e1), Some(e2), Some(e3)) // [e:e:e]
      case _                                                       => report(UnexpectedToken(nextToken, lastToken))
    }
    // @formatter:on
  }

  /**
    * <superCall> ::= super [ "<" <classIdentifier> "> ]
    */
  def superCall: ExprTree = positioned {
    eat(SUPER)
    val specifier = optional({
      val id = classIdentifier
      eat(GREATERTHAN)
      id
    }, LESSTHAN)
    Super(specifier)
  }

  private def replaceExprWithReturnStat(stat: StatTree): StatTree = {
    val s = stat match {
      case Block(stmts) if stmts.nonEmpty =>
        val replaced = replaceExprWithReturnStat(stmts.last)
        Block(stmts.updated(stmts.size - 1, replaced))
      case acc@Access(_, _: MethodCall)   => Return(Some(acc))
      case UselessStatement(e)            => Return(Some(e))
      case _                              => stat
    }
    s.setPos(stat)
  }

  /**
    * Parses expressions of type
    * E ::= <next> { ( kinds[0] | kinds[1] | ... | kinds[n] ) <next> }.
    * Used to parse left associative expressions. *
    */
  private def leftAssociative(next: => ExprTree, kinds: TokenKind*): ExprTree = {
    val startPos = nextToken
    var expr = next
    while (kinds.contains(currentToken.kind)) {
      kinds foreach { kind =>
        if (currentToken.kind == kind) {
          eat(kind)
          expr = tokenToBinaryOperatorAST(kind)(expr, next).setPos(startPos, lastVisibleToken)
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
        report(WrongToken(nextToken, lastToken, k))
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
    * <classTypeIdentifier> ::= <identifier> [ "<" <identifier> { "," <identifier> } ">" ]
    */
  private def classTypeIdentifier: ClassID = positioned {
    nextToken match {
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
        ClassID(id.value, tIds)
      case _      => report(WrongToken(nextToken, lastToken, IDKIND))
    }
  }

  /**
    * <classIdentifier> ::= <identifier> { :: <identifier> } <templateList>
    */
  private def classIdentifier: ClassID = positioned {
    val ids = nonEmptyList(identifierName, COLON, COLON)
    val id = ids.mkString("::")
    ClassID(id, templateList)
  }

  /**
    * <templateList> ::= [ "<" <type> { "," <type> } ">" ]
    */
  private def templateList: List[TypeTree] = {
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
  private def identifierName: String = nextToken match {
    case id: ID =>
      eat(IDKIND)
      id.value
    case _      => report(WrongToken(nextToken, lastToken, IDKIND))
  }

  /**
    * <varIdentifier> ::= sequence of letters, digits and underscores, starting with a letter and which is not a keyword
    */
  private def varIdentifier: VariableID = positioned {
    nextToken match {
      case id: ID =>
        eat(IDKIND)
        VariableID(id.value)
      case _      => report(WrongToken(nextToken, lastToken, IDKIND))
    }
  }

  /**
    * <methodIdentifier> ::= sequence of letters, digits and underscores, starting with a letter and which is not a keyword
    */
  private def methodIdentifier: MethodID = positioned {
    nextToken match {
      case id: ID =>
        eat(IDKIND)
        MethodID(id.value)
      case _      => report(WrongToken(nextToken, lastToken, IDKIND))
    }
  }

  /**
    * <stringLit> ::= sequence of arbitrary characters, except new lines and "
    */
  private def stringLit: StringLit = positioned {
    nextToken match {
      case strlit: STRLIT =>
        eat(STRLITKIND)
        StringLit(strlit.value)
      case _              => report(WrongToken(nextToken, lastToken, STRLITKIND))
    }
  }

  /**
    * <intLit> ::= sequence of digits, with no leading zeros
    */
  private def intLit: IntLit = positioned {
    nextToken match {
      case intLit: INTLIT =>
        eat(INTLITKIND)
        IntLit(intLit.value)
      case _              => report(WrongToken(nextToken, lastToken, INTLITKIND))
    }
  }


  /**
    * <longLit> ::= sequence of digits, with no leading zeros ending with an 'l'
    */
  private def longLit: LongLit = positioned {
    nextToken match {
      case longLit: LONGLIT =>
        eat(LONGLITKIND)
        LongLit(longLit.value)
      case _                => report(WrongToken(nextToken, lastToken, LONGLITKIND))
    }
  }

  /**
    * <floatLit> ::= sequence of digits, optionally with a single '.' ending with an 'f'
    */
  private def floatLit: FloatLit = positioned {
    nextToken match {
      case floatLit: FLOATLIT =>
        eat(FLOATLITKIND)
        FloatLit(floatLit.value)
      case _                  => report(WrongToken(nextToken, lastToken, FLOATLITKIND))
    }
  }


  /**
    * <doubleLit> ::= sequence of digits, optionally with a single '.' ending with an 'f'
    */
  private def doubleLit: DoubleLit = positioned {
    nextToken match {
      case doubleLit: DOUBLELIT =>
        eat(DOUBLELITKIND)
        DoubleLit(doubleLit.value)
      case _                    => report(WrongToken(nextToken, lastToken, DOUBLELITKIND))
    }
  }

  /**
    * <charLit> ::= sequence of digits, optionally with a single '.' ending with an 'f'
    */
  private def charLit: CharLit = positioned {
    nextToken match {
      case charLit: CHARLIT =>
        eat(CHARLITKIND)
        CharLit(charLit.value)
      case _                => report(WrongToken(nextToken, lastToken, CHARLITKIND))
    }
  }

  /**
    * Parses lists of the form
    * <nonEmptyList> ::= <parse> { <delimiter_0> ... <delimiter_n> <parse> }
    */

  private def nonEmptyList[T](parse: => T, delimiters: TokenKind*): List[T] = {
    val arrBuff = new ArrayBuffer[T]()
    arrBuff += parse

    def matches = (0 until delimiters.size).forall(i => tokens(currentIndex + i).kind == delimiters(i))

    while (matches) {
      delimiters.foreach(eat(_))
      arrBuff += parse
    }
    arrBuff.toList
  }

  /**
    * Parses a commalist of the form
    * <commaList> ::= [ <parse> { "," <parse> } ]
    */
  private def commaList[T](parse: => T, stopSign: TokenKind = RPAREN): List[T] = {
    if (nextTokenKind == stopSign)
      return List()

    val arrBuff = new ArrayBuffer[T]()
    arrBuff += parse
    while (nextToken.kind == COMMA) {
      eatNewLines()
      readToken()
      arrBuff += parse
    }
    arrBuff.toList
  }

  /**
    * Parses an optional of the form
    * <optional> ::= [ parse ] and returns Option
    */
  private def optional[T <: Positioned](parse: => T, kinds: TokenKind*): Option[T] = {
    if (kinds.contains(nextTokenKind)) {
      eat(nextTokenKind)
      Some(parse)
    } else {
      None
    }
  }

  /**
    * Parses on of the given tokens and returns true if the first token was parsed
    * and false if the second token was parsed.
    * <oneOf> ::= <first> | <second>
    */
  private def oneOf(first: TokenKind, second: TokenKind): Boolean = {
    nextTokenKind match {
      case `first`  =>
        eat(first)
        true
      case `second` =>
        eat(second)
        false
      case _        => report(WrongToken(currentToken, lastToken, first, second))
    }
  }

  /**
    * Continues parsing until one of the given token kinds are encountered.
    */
  private def until[T](parse: => T, kinds: TokenKind*): List[T] = {
    until(!kinds.contains(nextTokenKind), parse)
  }

  /**
    * Continues parsing until a token different from the given tokens is encountered.
    */
  private def untilNot[T](parse: => T, kinds: TokenKind*): List[T] = {
    until(kinds.contains(nextTokenKind), parse)
  }

  private def until[T](condition: => Boolean, parse: => T): List[T] = {
    var arrBuff = new ArrayBuffer[T]()
    while (condition) {
      arrBuff += parse
    }
    arrBuff.toList
  }

  private def positioned[T <: Positioned](f: => T): T = {
    val startPos = nextToken
    f.setPos(startPos, lastVisibleToken)
  }

}
