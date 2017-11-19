package tlang.compiler.ast

import tlang.Context
import tlang.compiler.analyzer.Types.TUnit
import tlang.compiler.ast.Trees._
import tlang.compiler.imports.Imports
import tlang.compiler.lexer.Tokens._
import tlang.compiler.lexer._
import tlang.compiler.{CompilerPhase, DebugOutputFormatter}
import tlang.formatting.Formatting
import tlang.messages.{ErrorStringContext, Reporter}
import tlang.utils.Extensions._
import tlang.utils.{Logging, Positioned}

import scala.collection.mutable.ListBuffer

object Parsing extends CompilerPhase[List[Token], CompilationUnit] with Logging {

  def run(ctx: Context)(tokenList: List[List[Token]]): List[CompilationUnit] =
    tokenList map { tokens =>
      info"Parsing tokens of ${ tokens.head.sourceName }"
      val errorStringContext = ErrorStringContext(ctx.formatter)
      val astBuilder = Parser(ctx, errorStringContext, TokenStream(tokens))
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

case class Parser(ctx: Context, override val errorStringContext: ErrorStringContext, tokens: TokenStream) extends ParsingErrors {

  override val reporter: Reporter = ctx.reporter

  import Parser._

  /**
    * <goal> ::= [ <packageDeclaration> ] { <importDeclaration> } { (<classDeclaration> | <methodDeclaration> | <statement> } <EOF>
    */
  def compilationUnit: CompilationUnit = positioned {
    val pack = packageDeclaration
    val imp = untilNot(IMPORT)(importDeclaration)

    val code = until(EOF) {
      tokens.nextKind match {
        case CLASS | TRAIT | EXTENSION => classDeclaration
        case PUBDEF | PRIVDEF          =>
          positioned {
            val modifiers = methodModifiers
            method(modifiers + Static())
          }
        case _                         => statement
      }
    }

    val classes = createMainClass(code)

    val imports = Imports(ctx, errorStringContext, imp, pack, classes)
    CompilationUnit(pack, classes, imports)
  }

  private def createMainClass(code: List[Tree]) = {
    var classes = code.filterInstance[ClassDeclTree]
    val methods = code.filterInstance[MethodDeclTree]
    val stats = code.filterInstance[StatTree]

    if (stats.nonEmpty || methods.nonEmpty) {
      val mainName = tokens.current.source.map(_.mainName).getOrElse("MissingSource")
      val mainClass = classes.filterInstance[IDClassDeclTree].find(_.id.name == mainName) match {
        case Some(c) => c
        case None    =>
          val pos = if (stats.nonEmpty) stats.head else methods.head
          val mainClass = ClassDecl(ClassID(mainName), List(), List(), List()).setPos(pos, tokens.lastVisible)
          classes ::= mainClass
          mainClass
      }

      mainClass.methods :::= methods

      if (stats.nonEmpty) {
        val args = List(Formal(ArrayType(ClassID("java::lang::String", List())), VariableID("args")))
        val modifiers: Set[Modifier] = Set(Public(), Static())
        val mainMethod = MethodDecl(MethodID("main").setNoPos(), modifiers, args, Some(UnitType()), Some(Block(stats))).setPos(stats.head, tokens.lastVisible)
        mainClass.methods ::= mainMethod
      }
    }
    classes
  }

  /**
    * <packageDeclaration> ::= package <identifier> { . <identifier> }
    */
  def packageDeclaration: Package = positioned {
    tokens.nextKind match {
      case PACKAGE =>
        eat(PACKAGE)
        val address = nonEmptyList(COLON, COLON)(identifierName)
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
    while (tokens.nextKind == COLON) {
      eat(COLON, COLON)
      tokens.nextKind match {
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
    while (tokens.nextKind == COLON) {
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
    if (tokens.nextKind == EXTENSION)
      extensionDeclaration
    else
      classOrTraitDeclaration

  /**
    * <classOrTraitDeclaration> ::= (class|trait) <classTypeIdentifier> <parentsDeclaration>
    * ( "=" <indent> { <varDeclaration> } { <methodDeclaration> } <dedent> | <endStatement> )
    */
  def classOrTraitDeclaration: ClassDeclTree = positioned {
    val classOrTrait = oneOf(CLASS, TRAIT)
    val id = classTypeIdentifier
    val parents = parentsDeclaration
    val (vars, methods) = if (tokens.nextKind == EQSIGN) {
      eat(EQSIGN)
      eat(INDENT)
      val vars = untilNot(PUBVAR, PRIVVAR, PUBVAL, PRIVVAL) {
        val v = fieldDeclaration
        endStatement()
        v
      }
      val methods = untilNot(PRIVDEF, PUBDEF)(methodDeclaration)
      eat(DEDENT)
      (vars, methods)
    } else {
      endStatement()
      (Nil, Nil)
    }

    if (classOrTrait == CLASS)
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
    val methods = if (tokens.nextKind == EQSIGN) {
      eat(EQSIGN)
      eat(INDENT)
      val methods = untilNot(PRIVDEF, PUBDEF)(methodDeclaration)
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
  def parentsDeclaration: List[ClassID] = tokens.nextKind match {
    case COLON =>
      eat(COLON)
      nonEmptyList(COMMA)(classIdentifier)
    case _     => Nil
  }

  /**
    * <fieldDeclaration> ::= <fieldModifiers> <variableEnd>
    */
  def fieldDeclaration: VarDecl = positioned {
    varDeclEnd(fieldModifiers)
  }

  /**
    * <fieldModifiers> ::= ( Var | Val | (var | val [ protected ])) [ static ] [ implicit ]
    */
  def fieldModifiers: Set[Modifier] = {
    val startPos = tokens.next
    var modifiers: Set[Modifier] = tokens.nextKind match {
      case PUBVAR  =>
        eat(PUBVAR)
        Set(Public().setPos(startPos, tokens.lastVisible))
      case PRIVVAR =>
        eat(PRIVVAR)
        Set(protectedOrPrivate.setPos(startPos, tokens.lastVisible))
      case PUBVAL  =>
        eat(PUBVAL)
        Set(Public().setPos(startPos, tokens.lastVisible), Final().setPos(startPos, tokens.lastVisible))
      case PRIVVAL =>
        eat(PRIVVAL)
        Set(protectedOrPrivate.setPos(startPos, tokens.lastVisible), Final().setPos(startPos, tokens.lastVisible))
      case _       => report(WrongToken(tokens.next, tokens.last, PUBVAR, PRIVVAR, PUBVAL, PRIVVAL))
    }

    val pos = tokens.next
    tokens.nextKind match {
      case STATIC =>
        eat(STATIC)
        modifiers += Static().setPos(pos, tokens.lastVisible)
      case _      =>
    }
    modifiers
  }

  /**
    * <localVarDeclaration> ::= (var | val) <varDeclEnd>
    */
  def localVarDeclaration: VarDecl = positioned {
    val varOrVal = oneOf(PRIVVAR, PRIVVAL)
    val modifiers: Set[Modifier] = if (varOrVal == PRIVVAR) Set(Private()) else Set(Private(), Final())
    varDeclEnd(modifiers)
  }

  /**
    * <varDeclEnd> ::= <identifier> [ ":" <tpe> ] [ "=" <expression> ]
    */
  private def varDeclEnd(modifiers: Set[Modifier]): VarDecl = {
    val id = varIdentifier
    val typ = optional(COLON)(tpe)
    val init = optional(EQSIGN)(expression)
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
    tokens.nextKind match {
      case IDKIND => method(mods)
      case NEW    => constructor(mods)
      case _      => operator(mods)
    }
  }


  /**
    * <methodModifiers> ::= ( Def | def [ protected ])) [ static ] [ implicit ]
    */
  private def methodModifiers: Set[Modifier] = {
    val startPos = tokens.next


    var modifiers: Set[Modifier] = tokens.nextKind match {
      case PUBDEF  =>
        eat(PUBDEF)
        Set(Public().setPos(startPos, tokens.lastVisible))
      case PRIVDEF =>
        eat(PRIVDEF)
        Set(protectedOrPrivate.setPos(startPos, tokens.lastVisible))
      case _       => report(WrongToken(tokens.next, tokens.last, PUBDEF, PRIVDEF))
    }

    while (tokens.nextKind == STATIC || tokens.nextKind == IMPLICIT) {
      val pos = tokens.next
      val modifier = tokens.nextKind match {
        case STATIC   =>
          eat(STATIC)
          Static().setPos(pos, tokens.lastVisible)
        case IMPLICIT =>
          eat(IMPLICIT)
          Implicit().setPos(pos, tokens.lastVisible)
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
    val args = commaList(LPAREN, RPAREN)(formal)
    val retType = optional(COLON)(returnType)

    val methBody = methodBody
    MethodDecl(id, modifiers, args, retType, methBody)
  }

  /**
    * <constructor> ::= new "(" [ <formal> { "," <formal> } ] ")" <methodBody>
    */
  def constructor(modifiers: Set[Modifier]): ConstructorDecl = {
    val pos = tokens.next
    eat(NEW)
    val methId = MethodID("new").setPos(pos, tokens.lastVisible)
    val args = commaList(LPAREN, RPAREN)(formal)
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

    def minusOperator: (OperatorTree, List[Formal], Set[Modifier]) = {
      // Minus is a special case since it can be both a unary and a binary operator
      eat(MINUS)
      eat(LPAREN)
      val f1 = formal
      tokens.nextKind match {
        case COMMA =>
          eat(COMMA)
          val f2 = formal
          eat(RPAREN)
          val operatorType = Minus(Empty(), Empty()).setType(TUnit)
          (operatorType, List(f1, f2), modifiers + Static())
        case _     =>
          eat(RPAREN)
          val operatorType = Negation(Empty()).setType(TUnit)
          (operatorType, List(f1), modifiers + Static())
      }
    }

    def binaryOperator(constructor: (ExprTree, ExprTree) => OperatorTree): (OperatorTree, List[Formal], Set[Modifier]) = {
      eat(tokens.nextKind)
      val operatorType = constructor(Empty(), Empty()).setType(TUnit)
      eat(LPAREN)
      val f1 = formal
      eat(COMMA)
      val f2 = formal
      eat(RPAREN)
      (operatorType, List(f1, f2), modifiers + Static())
    }

    def unaryOperator(constructor: (ExprTree) => OperatorTree): (OperatorTree, List[Formal], Set[Modifier]) = {
      eat(tokens.nextKind)
      val operatorType: OperatorTree = constructor(Empty()).setType(TUnit)
      eat(LPAREN)
      val f = formal
      eat(RPAREN)
      (operatorType, List(f), modifiers + Static())
    }

    def indexingOperator = {
      eat(LBRACKET)

      val (numArgs, operatorType) = tokens.nextKind match {
        case RBRACKET =>
          eat(RBRACKET)
          tokens.nextKind match {
            case LPAREN =>
              (1, ArrayRead(Empty(), Empty()))
            case EQSIGN =>
              eat(EQSIGN)
              (2, Assign(ArrayRead(Empty(), Empty()), Empty()))
            case _      => report(WrongToken(tokens.next, tokens.last, EQSIGN, LPAREN))
          }
        case COLON    =>
          eat(COLON, COLON, RBRACKET)
          (3, ArraySlice(Empty(), None, None, None))
        case _        => report(WrongToken(tokens.next, tokens.last, RBRACKET, COLON))
      }

      modifiers.findInstance[Static].ifDefined { static =>
        report(StaticIndexingOperator(static))
      }

      val args = commaList(LPAREN, RPAREN)(formal)
      if (args.size != numArgs)
        report(UnexpectedToken(tokens.current, tokens.last))

      (operatorType, args, modifiers)
    }

    val (operatorType, args, newModifiers) = tokens.nextKind match {
      case MINUS         => minusOperator
      case PLUS          => binaryOperator(Plus)
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
      case LBRACKET      => indexingOperator
      case _             =>
        report(WrongToken(tokens.next, tokens.last, PLUS, MINUS, TIMES, DIV, MODULO, LOGICAND, LOGICOR, LOGICXOR, LEFTSHIFT,
          RIGHTSHIFT, LESSTHAN, LESSTHANEQ, GREATERTHAN, GREATERTHANEQ, EQUALS, NOTEQUALS, INCREMENT, DECREMENT,
          LOGICNOT, LBRACKET))
    }

    val retType = optional(COLON)(returnType)
    val methBody = methodBody

    OperatorDecl(operatorType.setNoPos(), newModifiers, args, retType, methBody)
  }


  /**
    * [ "=" <statement> ]
    */
  def methodBody: Option[StatTree] = optional(EQSIGN) { replaceExprWithReturnStat(statement) }

  private def protectedOrPrivate: Accessability = positioned {
    tokens.nextKind match {
      case PROTECTED =>
        eat(PROTECTED)
        Protected()
      case _         => Private()
    }
  }

  /**
    * <tpe> ::= <classIdentifier> { "[]" | "?" }
    */
  def tpe: TypeTree = {
    val startPos = tokens.next
    var e: TypeTree = classIdentifier
    var dimension = 0

    while (tokens.nextKind in List(QUESTIONMARK, LBRACKET)) {
      e = tokens.nextKind match {
        case QUESTIONMARK =>
          eat(QUESTIONMARK)
          NullableType(e)
        case LBRACKET     =>
          eat(LBRACKET, RBRACKET)
          dimension += 1
          ArrayType(e)
        case _            => ???
      }
      e.setPos(startPos, tokens.lastVisible)
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
    * | <forLoop> <endStatement>
    * | (print|println|error)"(" [ <expression> ] ")" <endStatement>
    * | break
    * | continue
    * | return [ <expression> ] <endStatement>
    * | <expression> <endStatement>
    **/
  def statement: StatTree = positioned {
    // Variable needs custom end position in order to
    // only highlight expression up to equals sign
    tokens.nextKind match {
      case PRIVVAR | PRIVVAL =>
        val variable = localVarDeclaration
        endStatement()
        return variable
      case _                 =>
    }

    tokens.nextKind match {
      case INDENT                  =>
        eat(INDENT)
        val statements = until(DEDENT)(statement)
        eat(DEDENT)
        Block(statements)
      case IF                      =>
        eat(IF, LPAREN)
        val condition = expression
        eat(RPAREN)
        val stmt = statement
        val els = optional(ELSE)(statement)
        If(condition, stmt, els)
      case RETURN                  =>

        eat(RETURN)
        val expr = if (tokens.current.kind != SEMICOLON && tokens.current.kind != NEWLINE) Some(expression) else None
        endStatement()
        Return(expr)
      case PRINT | PRINTLN | ERROR =>

        val methType = tokens.nextKind
        eat(methType)
        eat(LPAREN)
        val expr = tokens.nextKind match {
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
      case FOR                     =>
        forLoop
      case WHILE                   =>
        eat(WHILE, LPAREN)
        val condition = expression
        eat(RPAREN)
        While(condition, statement)
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
    * <forLoop> ::= for "(" <forInit> ";" [ <expression> ] ";" <forIncrement> ")" <statement>
    */
  def forLoop: StatTree = positioned {
    eat(FOR, LPAREN)

    tokens.nextKind match {
      case PRIVVAR | PRIVVAL =>
        val varDecl = localVarDeclaration
        if (varDecl.initiation.isDefined) {
          if (tokens.nextKind == COMMA)
            eat(COMMA)
          regularForLoop(Some(varDecl))
        } else {
          tokens.nextKind match {
            case IN =>
              forEachLoop(varDecl)
            case _  =>
              if (tokens.nextKind == COMMA)
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
    val condition = tokens.nextKind match {
      case SEMICOLON => TrueLit() // if condition is left out, use 'true'
      case _         => expression
    }
    val post = commaList(SEMICOLON, RPAREN)(expression)
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
    commaList(None, Some(SEMICOLON)) {
      val startPos = tokens.next
      tokens.nextKind match {
        case PRIVVAR =>
          localVarDeclaration
        case _       =>
          val id = varIdentifier
          tokens.nextKind match {
            case EQSIGN | PLUSEQ | MINUSEQ | DIVEQ | MODEQ | ANDEQ | OREQ | XOREQ | LEFTSHIFTEQ | RIGHTSHIFTEQ =>
              assignment(Some(id)).asInstanceOf[Assign].setPos(startPos, tokens.lastVisible)
            case _                                                                                             =>
              report(WrongToken(tokens.next, tokens.last, EQSIGN, PLUSEQ, MINUSEQ, MULEQ, DIVEQ, MODEQ,
                ANDEQ, OREQ, XOREQ, LEFTSHIFTEQ, RIGHTSHIFTEQ))
          }
      }
    }

  /**
    * <endStatement> ::= ( ; | \n ) { ; | \n }
    */
  def endStatement(): Unit = tokens.current.kind match {
    case SEMICOLON | NEWLINE =>
      tokens.readNext()
      while (tokens.current.kind == SEMICOLON || tokens.current.kind == NEWLINE)
        tokens.readNext()
    case EOF | RBRACE        =>
    case _                   => report(WrongToken(tokens.next, tokens.last, SEMICOLON, NEWLINE))
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
    val startPos = tokens.next
    val e = expr.getOrElse(ternary)

    def assignment(constructor: Option[(ExprTree, ExprTree) => ExprTree]) = {
      eat(tokens.nextKind)

      val assignmentExpr = constructor
        .map(cons => cons(e, expression).setPos(startPos, tokens.lastVisible))
        .getOrElse(expression)

      e match {
        case a: Assignable => Assign(a, assignmentExpr)
        case _             => report(ExpectedIdAssignment(e))
      }
    }

    tokens.nextKind match {
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
    if (tokens.nextKind == QUESTIONMARK) {
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
    if (tokens.nextKind == ELVIS) {
      eat(ELVIS)
      val ifNull = or
      Elvis(e, ifNull)
    } else {
      e
    }
  }

  /** <or> ::= <and> { || <and> } */
  def or: ExprTree = leftAssociative(OR)(and)

  /** <and> ::= <logicOr> { && <logicOr> } */
  def and: ExprTree = leftAssociative(AND)(logicOr)

  /** <logicOr> ::= <logicXor> { | <logicXor> } */
  def logicOr: ExprTree = leftAssociative(LOGICOR)(logicXor)

  /** <logicXor> ::= <logicAnd> { ^ <logicAnd> } */
  def logicXor: ExprTree = leftAssociative(LOGICXOR)(logicAnd)

  /** <logicAnd> ::= <eqNotEq> { & <eqNotEq> } */
  def logicAnd: ExprTree = leftAssociative(LOGICAND)(eqNotEq)

  /** <eqNotEq> ::= <is> { ( == | != ) <is> } */
  def eqNotEq: ExprTree = leftAssociative(EQUALS, NOTEQUALS)(is)

  /** <is> ::= <comparison> { inst <classIdentifier> } */
  def is: ExprTree = positioned {
    var e = comparison
    while (tokens.nextKind == IS) {
      eat(IS)
      e = Is(e, tpe)
    }
    e
  }

  /** <comparison> ::= <bitShift> { ( < | <= | > | >= | inst ) <bitShift> } */
  def comparison: ExprTree = leftAssociative(LESSTHAN, LESSTHANEQ, GREATERTHAN, GREATERTHANEQ)(bitShift)

  /** <bitShift> ::= <plusMinus> { ( << | >> ) <plusMinus> } */
  def bitShift: ExprTree = leftAssociative(LEFTSHIFT, RIGHTSHIFT)(plusMinus)

  /** <plusMinus> ::= <timesDiv> { ( + | - ) <timesDiv> } */
  def plusMinus: ExprTree = leftAssociative(PLUS, MINUS)(timesDivMod)

  /** <timesDivMod> ::= <term> { ( * | / | % ) <term> } */
  def timesDivMod: ExprTree = leftAssociative(TIMES, DIV, MODULO)(term)

  /** <term> ::= <termFirst> { termRest } */
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
    // These are sorted by commonness. For instance IDKIND and INTLIT are the by
    // far most common terms, thats why they appear first.
    tokens.nextKind match {
      case IDKIND        =>
        val methStartPos = tokens.next
        val ids = nonEmptyList(COLON, COLON)(identifierName)
        val name = ids.mkString("::")
        tokens.nextKind match {
          case LPAREN =>
            val id = MethodID(name).setPos(methStartPos, tokens.lastVisible)
            val exprs = commaList(LPAREN, RPAREN)(expression)
            val meth = MethodCall(id, exprs).setPos(methStartPos, tokens.lastVisible)
            NormalAccess(Empty(), meth)
          case _      => VariableID(name)
        }
      case INTLITKIND    => literal(INTLITKIND, IntLit)
      case STRLITKIND    => literal(STRLITKIND, StringLit)
      case CHARLITKIND   => literal(CHARLITKIND, CharLit)
      case LONGLITKIND   => literal(LONGLITKIND, LongLit)
      case DOUBLELITKIND => literal(DOUBLELITKIND, DoubleLit)
      case FLOATLITKIND  => literal(FLOATLITKIND, FloatLit)
      case NEW           => newExpression
      case LPAREN        =>
        eat(LPAREN)
        val expr = expression
        eat(RPAREN)
        expr
      case LBRACKET      =>
        val expressions = commaList(LBRACKET, RBRACKET)(expression)
        ArrayLit(expressions)
      case NULL          =>
        eat(NULL)
        NullLit()
      case THIS          =>
        eat(THIS)
        This()
      case TRUE          =>
        eat(TRUE)
        TrueLit()
      case FALSE         =>
        eat(FALSE)
        FalseLit()
      case BANG          =>
        eat(BANG)
        Not(term)
      case MINUS         =>
        negation
      case LOGICNOT      =>
        eat(LOGICNOT)
        LogicNot(term)
      case HASH          =>
        eat(HASH)
        Hash(term)
      case INCREMENT     =>
        eat(INCREMENT)
        PreIncrement(term)
      case DECREMENT     =>
        eat(DECREMENT)
        PreDecrement(term)
      case SUPER         =>
        val sup = superCall
        access(sup)
      case _             =>
        report(UnexpectedToken(tokens.current, tokens.last))
    }
  }

  private val restTokens = List(DOT, SAFEACCESS, EXTRACTNULLABLE, LBRACKET, AS, INCREMENT, DECREMENT)
  /**
    * <termRest> ::= <access>
    * | <arrayIndexing>
    * | as <tpe>
    * | ++
    * | --
    * | !!
    */
  def termRest(termFirst: ExprTree): ExprTree = {
    var e = termFirst
    // Uses current token since a newline should stop the iteration
    while (tokens.current.kind in restTokens) {
      e = tokens.current.kind match {
        case DOT | SAFEACCESS =>
          access(e)
        case LBRACKET         =>
          arrayIndexing(e)
        case INCREMENT        =>
          eat(INCREMENT)
          PostIncrement(e)
        case DECREMENT        =>
          eat(DECREMENT)
          PostDecrement(e)
        case AS               =>
          eat(AS)
          As(e, tpe)
        case EXTRACTNULLABLE  =>
          eat(EXTRACTNULLABLE)
          ExtractNullable(e)
        case _                => ???
      }
      e.setPos(termFirst, tokens.lastVisible)
    }
    e
  }

  /**
    * <access> ::= (. | ?.) <methodCall> | <identifier>
    */
  def access(obj: ExprTree): Access = positioned {
    val access = tokens.nextKind match {
      case SAFEACCESS =>
        eat(SAFEACCESS)
        SafeAccess
      case DOT        =>
        eat(DOT)
        NormalAccess
      case _          => report(WrongToken(tokens.next, tokens.last, DOT, QUESTIONMARK))
    }

    val application = positioned {
      val id = varIdentifier
      tokens.nextKind match {
        case LPAREN =>
          val exprs = commaList(LPAREN, RPAREN)(expression)
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
    tokens.current match {
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
    val startPos = tokens.next
    eat(NEW)
    val tpe: TypeTree = classIdentifier

    tokens.nextKind match {
      case LPAREN                  =>
        val args = commaList(LPAREN, RPAREN) { expression }
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
        if (tokens.nextKind == QUESTIONMARK) {
          eat(QUESTIONMARK)
          e = NullableType(e).setPos(startPos, tokens.lastVisible)
          _nullableBracket()
        }

        while (tokens.nextKind == LBRACKET)
          _nullableBracket()

        NewArray(e, sizes.toList)
      case _                       => report(WrongToken(tokens.next, tokens.last, LPAREN, LBRACKET))
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
    e = ArrayType(e).setPos(startPos, tokens.lastVisible)
    if (tokens.nextKind == QUESTIONMARK) {
      eat(QUESTIONMARK)
      e = NullableType(e).setPos(startPos, tokens.lastVisible)
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
    val exprs = until(RBRACKET) {
      tokens.nextKind match {
        case COLON =>
          eat(COLON)
          None
        case _     =>
          Some(expression)
      }
    }
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
      case _                                                       => report(UnexpectedToken(tokens.next, tokens.last))
    }
    // @formatter:on
  }

  /**
    * <superCall> ::= super [ "<" <classIdentifier> "> ]
    */
  def superCall: ExprTree = positioned {
    eat(SUPER)
    val specifier = optional(LESSTHAN) {
      val id = classIdentifier
      eat(GREATERTHAN)
      id
    }
    Super(specifier)
  }

  /** Eats the expected tokens, or terminates with an error. */
  private def eat(kind: TokenKind*): Unit = {
    tokens.readNewLines()
    for (k <- kind) tokens.nextKind match {
      case `k` => tokens.readNext()
      case _   => report(WrongToken(tokens.next, tokens.last, k))
    }
  }


  /**
    * Handles the conflict of generics having multiple ">" signs by
    * treating RIGHTSHIFT (>>) as two ">".
    */
  private var usedOneGreaterThan = false
  private def eatRightShiftOrGreaterThan(): Unit = {
    if (tokens.nextKind != RIGHTSHIFT) {
      eat(GREATERTHAN)
      return
    }
    if (usedOneGreaterThan)
      eat(RIGHTSHIFT)

    usedOneGreaterThan = !usedOneGreaterThan
  }


  private def replaceExprWithReturnStat(stat: StatTree): StatTree = {
    val s = stat match {
      case Block(statements) if statements.nonEmpty =>
        val replaced = replaceExprWithReturnStat(statements.last)
        Block(statements.updated(statements.size - 1, replaced))
      case acc@Access(_, _: MethodCall)             => Return(Some(acc))
      case UselessStatement(e)                      => Return(Some(e))
      case _                                        => stat
    }
    s.setPos(stat)
  }

  /**
    * Parses expressions of type
    * E ::= <next> { ( kinds[0] | kinds[1] | ... | kinds[n] ) <next> }.
    * Used to parse left associative expressions. *
    */
  private def leftAssociative(kinds: TokenKind*)(next: => ExprTree): ExprTree = {
    val startPos = tokens.next

    def matchingKind = kinds.find(_ == tokens.current.kind)

    var expr = next
    var kind = matchingKind
    while (kind.isDefined) {
      eat(kind.get)
      expr = tokenToBinaryOperatorAST(kind.get)(expr, next).setPos(startPos, tokens.lastVisible)
      kind = matchingKind
    }
    expr
  }


  /**
    * <classTypeIdentifier> ::= <identifier> [ "<" <identifier> { "," <identifier> } ">" ]
    */
  def classTypeIdentifier: ClassID = positioned {
    tokens.next match {
      case id: ID =>
        eat(IDKIND)
        val tIds = tokens.nextKind match {
          case LESSTHAN =>
            val tmp = commaList(Some(LESSTHAN), None)(varIdentifier)
            eatRightShiftOrGreaterThan()
            tmp.map(x => ClassID(x.name, List()).setPos(x))
          case _        => List()
        }
        ClassID(id.value, tIds)
      case _      => report(WrongToken(tokens.next, tokens.last, IDKIND))
    }
  }

  /**
    * <classIdentifier> ::= <identifier> { :: <identifier> } <templateList>
    */
  private def classIdentifier: ClassID = positioned {
    val ids = nonEmptyList(COLON, COLON)(identifierName)
    val id = ids.mkString("::")
    ClassID(id, templateList)
  }

  /**
    * <templateList> ::= [ "<" <type> { "," <type> } ">" ]
    */
  private def templateList: List[TypeTree] = {
    tokens.nextKind match {
      case LESSTHAN =>
        val tmp = commaList(Some(LESSTHAN), None)(tpe)
        eatRightShiftOrGreaterThan()
        tmp
      case _        => List()
    }
  }

  /**
    * Fetches the name of the identifer without creating an identifier object.
    */
  private def identifierName: String = tokens.next match {
    case id: ID =>
      eat(IDKIND)
      id.value
    case _      => report(WrongToken(tokens.next, tokens.last, IDKIND))
  }

  /**
    * <varIdentifier> ::= sequence of letters, digits and underscores, starting with a letter and which is not a keyword
    */
  private def varIdentifier: VariableID = positioned {
    tokens.next match {
      case id: ID =>
        eat(IDKIND)
        VariableID(id.value)
      case _      => report(WrongToken(tokens.next, tokens.last, IDKIND))
    }
  }

  /**
    * <methodIdentifier> ::= sequence of letters, digits and underscores, starting with a letter and which is not a keyword
    */
  private def methodIdentifier: MethodID = positioned {
    tokens.next match {
      case id: ID =>
        eat(IDKIND)
        MethodID(id.value)
      case _      => report(WrongToken(tokens.next, tokens.last, IDKIND))
    }
  }

  /**
    * Parses a literal of the given kind, producing the given literalType
    * e.g. literal(STRLITKIND, StringLit)
    *
    * The kind has to be of a TokenWithValue kind.
    */
  private def literal[T, Lit <: Literal[T]](kind: TokenKind, literalType: T => Lit): Lit = positioned {
    val next = tokens.next
    if (kind.getClass.isInstance(next.kind)) {
      eat(kind)
      val tokenValue = next.asInstanceOf[TokenWithValue[T]].value
      literalType(tokenValue)
    } else {
      report(WrongToken(next, tokens.last, kind))
    }
  }

  /**
    * Parses lists of the form
    * <nonEmptyList> ::= <parse> { <delimiter_0> ... <delimiter_n> <parse> }
    */

  private def nonEmptyList[T](delimiters: TokenKind*)(parse: => T): List[T] = {
    val b = new ListBuffer[T]()
    b += parse

    def matches = (0 until delimiters.size).forall(i => tokens.offset(i).kind == delimiters(i))

    while (matches) {
      delimiters.foreach(eat(_))
      b += parse
    }
    b.toList
  }


  /**
    * Parses a comma separated list of items of the form
    * <commaList> ::= <startToken> [ <parse> { "," <parse> } ] <endToken>
    */
  private def commaList[T](startToken: Option[TokenKind], stopToken: Option[TokenKind])(parse: => T): List[T] = {
    startToken.foreach(eat(_))

    if (stopToken contains tokens.nextKind) {
      stopToken.foreach(eat(_))
      return List()
    }

    val b = new ListBuffer[T]()
    b += parse
    while (tokens.next.kind == COMMA) {
      tokens.readNewLines()
      tokens.readNext()
      b += parse
    }
    stopToken.foreach(eat(_))

    b.toList
  }

  private def commaList[T](startToken: TokenKind, stopToken: TokenKind)(parse: => T): List[T] = {
    commaList(Some(startToken), Some(stopToken))(parse)
  }


  /**
    * Parses an optional of the form
    * <optional> ::= [ parse ] and returns Option
    */
  private def optional[T <: Positioned](kinds: TokenKind*)(parse: => T): Option[T] = {
    if (kinds.contains(tokens.nextKind)) {
      eat(tokens.nextKind)
      Some(parse)
    } else {
      None
    }
  }

  /**
    * Parses on of the given tokens and returns the matching tokenKind.
    */
  private def oneOf(kinds: TokenKind*): TokenKind = {
    val nextKind = tokens.nextKind
    kinds
      .find { _ == nextKind }
      .map { kind =>
        eat(kind)
        kind
      }
      .getOrElse { report(WrongToken(tokens.current, tokens.last, kinds.head, kinds.tail: _*)) }
  }

  /**
    * Continues parsing until one of the given token kinds are encountered.
    */
  private def until[T](kinds: TokenKind*)(parse: => T): List[T] = {
    until(!kinds.contains(tokens.nextKind))(parse)
  }

  /**
    * Continues parsing until a token different from the given tokens is encountered.
    */
  private def untilNot[T](kinds: TokenKind*)(parse: => T): List[T] = {
    until(kinds.contains(tokens.nextKind))(parse)
  }

  private def until[T](condition: => Boolean)(parse: => T): List[T] = {
    var b = new ListBuffer[T]()
    while (condition) b += parse
    b.toList
  }

  private def positioned[T <: Positioned](f: => T): T = {
    val startPos = tokens.next
    val res = f
    res.setPos(startPos, tokens.lastVisible)
  }

}
