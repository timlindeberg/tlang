package tlang.compiler.ast

import sourcecode.{Enclosing, Line}
import tlang.compiler.analyzer.Naming.phaseName
import tlang.compiler.analyzer.Types.TUnit
import tlang.compiler.ast.Trees._
import tlang.compiler.imports.Imports
import tlang.compiler.lexer.Tokens.{LPAREN, _}
import tlang.compiler.lexer._
import tlang.compiler.messages.Reporter
import tlang.compiler.output.Output
import tlang.compiler.output.debug.ASTOutput
import tlang.compiler.{CompilerPhase, Context}
import tlang.formatting.{ErrorStringContext, Formatting}
import tlang.utils.Extensions._
import tlang.utils.LogLevel.{Debug, Trace}
import tlang.utils.{Logging, NoPosition, Positioned}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object Parsing extends CompilerPhase[List[Token], CompilationUnit] with Logging {

  def run(ctx: Context)(tokenList: List[List[Token]]): List[CompilationUnit] =
    ctx.executor.map(tokenList) { tokens =>
      info"Parsing tokens of ${ tokens.head.source }"
      val errorStringContext = ErrorStringContext(ctx.formatter)
      val astBuilder = Parser(ctx, errorStringContext, TokenStream(tokens))
      astBuilder.compilationUnit
    }

  override def description(formatting: Formatting): String =
    "Parses the tokens produced by the lexing phase and generates an AST."

  override def debugOutput(output: List[CompilationUnit]): Output = ASTOutput(phaseName, output)

}

object Parser {

  val MaximumArraySize = 255

  private val TokenToBinaryOperatorAST: Map[TokenKind, (ExprTree, ExprTree) => ExprTree] = Map(
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
    LSHIFT -> LeftShift,
    RSHIFT -> RightShift,
    LOGICAND -> LogicAnd,
    LOGICOR -> LogicOr,
    LOGICXOR -> LogicXor
  )

}

case class Parser(ctx: Context, override val errorStringContext: ErrorStringContext, tokens: TokenStream) extends ParsingErrors with Logging {

  override val reporter: Reporter = ctx.reporter
  private  var indent             = 0

  import Parser._


  //----------------------------------------------------------------------------------------------
  //--- Declarations
  //----------------------------------------------------------------------------------------------


  /** <compilationUnit> ::=
    * [ <packageDeclaration> <statementEnd>  ]
    * { <importDeclaration> <statementEnd> }
    * { <classDeclaration> <statementEnd> | <methodDeclaration> <statementEnd> | <statement> <statementEnd> }
    * <EOF>
    */
  def compilationUnit: CompilationUnit = positioned {
    val pack = tokens.next.kind match {
      case PACKAGE => packageDeclaration after statementEnd
      case _       => Package(Nil)
    }

    val imps = untilNot(IMPORT) { importDeclaration after statementEnd }

    val code = until(EOF) {
      tokens.next.kind match {
        case CLASS | TRAIT | EXTENSION => classDeclaration after statementEnd
        case PUBDEF | PRIVDEF          =>
          positioned {
            val modifiers = methodModifiers
            method(modifiers + Static().setPos(modifiers.head)) after statementEnd
          }
        case _                         => statement after statementEnd
      }
    }

    val classes = createClasses(code)
    val imports = createImports(imps, pack, classes)

    CompilationUnit(pack, classes, imports)
  }

  private def createImports(imps: List[Import], pack: Package, classes: List[ClassDeclTree]): Imports = {
    val imports = Imports(ctx, errorStringContext, imps)
    if (pack.isEmpty)
      return imports

    // If we have a package we add a mapping from the shortname to the full name,
    // C -> A::B::C
    val packageName = pack.name
    classes
      .filterInstance[IDClassDeclTree]
      .map { _.id.name }
      .foreach { name => imports += (name, s"$packageName::$name") }
    imports
  }

  private def createClasses(code: List[Tree]): List[ClassDeclTree] = {
    val classes = code.filterInstance[ClassDeclTree]
    val methods = code.filterInstance[MethodDeclTree]
    val stats = code.filterInstance[StatTree]

    if (stats.isEmpty && methods.isEmpty)
      return classes

    val mainName = tokens.source.map(_.mainName).getOrElse("MissingSource")

    classes.filterInstance[ClassDecl].find(_.id.name == mainName) ifDefined { mainClass =>
      val pos = if (stats.nonEmpty) stats.head else methods.head
      report(FileClassAlreadyDefined(mainName, mainClass, pos))
    }

    if (stats.isEmpty)
      return createMainClass(mainName, methods) :: classes

    methods.find(_.id.name == "main") ifDefined { mainMethod =>
      report(MainMethodAlreadyDefined(mainMethod, stats.head))
    }

    val mainMethod = createMainMethod(stats)
    createMainClass(mainName, mainMethod :: methods) :: classes
  }

  private def createMainClass(name: String, methods: List[MethodDeclTree]) = {
    val start = methods.headOption.getOrElse(NoPosition)
    val end = methods.lastOption.getOrElse(NoPosition)
    ClassDecl(ClassID(name), methods = methods).setPos(start, end)
  }

  private def createMainMethod(stats: List[StatTree]): MethodDecl = {
    val args = List(Formal(ArrayType(ClassID("java::lang::String", List())), VariableID("args")))
    val modifiers: Set[Modifier] = Set(Public(), Static())
    MethodDecl(MethodID("main").setNoPos(), modifiers, args, Some(UnitType()), Some(Block(stats))).setPos(stats.head, tokens.lastVisible)
  }

  /** <packageDeclaration> ::= package <identifierName> { :: <identifierName> } */
  def packageDeclaration: Package = positioned {
    eat(PACKAGE)
    val address = nonEmptyList(COLON, COLON)(identifierName)
    Package(address)
  }

  /** <importDeclaration> ::= import <identifierName> { :: ( <identifierName> | * | extension  ) } */
  def importDeclaration: Import = positioned {
    eat(IMPORT)
    val address = ListBuffer[String](identifierName)
    var imp: Option[Import] = None
    while (tokens.next.kind == COLON) {
      eat(COLON, COLON)
      tokens.next.kind match {
        case TIMES     =>
          eat(TIMES)
          imp = Some(WildCardImport(address.toList))
        case EXTENSION =>
          imp = Some(extensionImportDeclaration(address.toList))
        case _         => address += identifierName
      }
    }
    imp.getOrElse(RegularImport(address.toList))
  }

  /** <extensionImportDeclaration> ::= extension <identifierName> { :: <identifierName> } */
  def extensionImportDeclaration(address: List[String]): Import = {
    eat(EXTENSION)

    val className = nonEmptyList(COLON, COLON)(identifierName)
    ExtensionImport(address, className)
  }

  /** <classDeclaration> ::=
    * | <classOrTraitDeclaration>
    * | <extensionDeclaration>
    */
  def classDeclaration: ClassDeclTree =
    if (tokens.next.kind == EXTENSION)
      extensionDeclaration
    else
      classOrTraitDeclaration

  /** <classOrTraitDeclaration> ::= (class|trait) <classTypeIdentifier> <parentsDeclaration>
    * [ = <indent>
    *    { <fieldDeclaration> <statementEnd> }
    *    { <methodDeclaration> <statementEnd> }
    *    <dedent>
    * ]
    */
  def classOrTraitDeclaration: ClassDeclTree = positioned {
    val classOrTrait = tokens.next.kind match {
      case CLASS => CLASS
      case TRAIT => TRAIT
      case _     => report(WrongToken(tokens.next, tokens.last, CLASS, TRAIT))
    }
    eat(classOrTrait)

    val id = classTypeIdentifier
    val parents = parentsDeclaration
    val (vars, methods) = tokens.next.kind match {
      case EQSIGN =>
        eat(EQSIGN)
        eat(INDENT)
        val vars = untilNot(PUBVAR, PRIVVAR, PUBVAL, PRIVVAL) { fieldDeclaration after statementEnd }
        val methods = untilNot(PRIVDEF, PUBDEF) { methodDeclaration after statementEnd }
        eat(DEDENT)
        (vars, methods)
      case _      => (Nil, Nil)
    }

    if (classOrTrait == CLASS)
      ClassDecl(id, parents, vars, methods)
    else
      TraitDecl(id, parents, vars, methods)
  }

  /** <extensionDeclaration> ::= extension <tpe> [ = <indent> { <methodDeclaration> <statementEnd> } <dedent> ] */
  def extensionDeclaration: ExtensionDecl = positioned {
    eat(EXTENSION)
    val id = tpe
    val methods = tokens.next.kind match {
      case EQSIGN =>
        eat(EQSIGN)
        eat(INDENT)
        val methods = untilNot(PRIVDEF, PUBDEF) { methodDeclaration after statementEnd }
        eat(DEDENT)
        methods
      case _      => Nil
    }
    ExtensionDecl(id, methods)
  }

  /** <parentsDeclaration> ::= [ : <classType> { "," <classType> } ] */
  def parentsDeclaration: List[ClassID] = tokens.next.kind match {
    case COLON =>
      eat(COLON)
      nonEmptyList(COMMA)(classType)
    case _     => Nil
  }

  /** <fieldDeclaration> ::= <fieldModifiers> <varDeclEnd> */
  def fieldDeclaration: VarDecl = positioned {
    varDeclEnd(fieldModifiers)
  }

  /** <fieldModifiers> ::= ( Var | Val | (var | val [ protected ])) [ static } */
  def fieldModifiers: Set[Modifier] = {
    val startPos = tokens.next
    val modifiers = mutable.Set[Modifier]()

    tokens.next.kind match {
      case PUBVAR  =>
        eat(PUBVAR)
        modifiers += Public().setPos(startPos, tokens.lastVisible)
      case PUBVAL  =>
        eat(PUBVAL)
        modifiers += Public().setPos(startPos, tokens.lastVisible)
        modifiers += Final().setPos(startPos, tokens.lastVisible)
      case PRIVVAR =>
        eat(PRIVVAR)
        modifiers += protectedOrPrivate.setPos(startPos, tokens.lastVisible)
      case PRIVVAL =>
        eat(PRIVVAL)
        modifiers += protectedOrPrivate.setPos(startPos, tokens.lastVisible)
        modifiers += Final().setPos(startPos, tokens.lastVisible)
      case _       => report(WrongToken(tokens.next, tokens.last, PUBVAR, PRIVVAR, PUBVAL, PRIVVAL))
    }

    val pos = tokens.next
    tokens.next.kind match {
      case STATIC =>
        eat(STATIC)
        modifiers += Static().setPos(pos, tokens.lastVisible)
      case _      =>
    }
    modifiers.toSet
  }

  /** <varDeclaration> ::= (var | val) <varDeclEnd> */
  def varDeclaration: VarDecl = positioned {
    val startPos = tokens.next
    val modifiers: Set[Modifier] = tokens.next.kind match {
      case PRIVVAR =>
        eat(PRIVVAR)
        Set(Private())
      case PRIVVAL =>
        eat(PRIVVAL)
        Set(Private(), Final())
      case _       => report(WrongToken(tokens.next, tokens.last, PRIVVAR, PRIVVAL))
    }
    modifiers.foreach(_.setPos(startPos, tokens.lastVisible))
    varDeclEnd(modifiers)
  }

  /** <varDeclEnd> ::= <identifier> [ : <tpe> ] [ = <expression> ] */
  def varDeclEnd(modifiers: Set[Modifier]): VarDecl = {
    val id = identifier(VariableID)
    val typ = optional(COLON)(tpe)
    val init = optional(EQSIGN)(expression)
    VarDecl(id, typ, init, modifiers)
  }

  /** <methodDeclaration> ::= <methodModifiers> ( <method> | <constructor> | <operator> ) */
  def methodDeclaration: MethodDeclTree = positioned {
    val mods = methodModifiers
    tokens.next.kind match {
      case IDKIND => method(mods)
      case NEW    => constructor(mods)
      case _      => operator(mods)
    }
  }

  /** <methodModifiers> ::= ( Def | def [ protected ])) { static | implicit } */
  def methodModifiers: Set[Modifier] = {
    val startPos = tokens.next
    val modifiers = mutable.Set[Modifier]()
    val accessability = tokens.next.kind match {
      case PUBDEF  =>
        eat(PUBDEF)
        Public()
      case PRIVDEF =>
        eat(PRIVDEF)
        protectedOrPrivate
      case _       => report(WrongToken(tokens.next, tokens.last, PUBDEF, PRIVDEF))
    }

    modifiers += accessability.setPos(startPos, tokens.lastVisible)

    while (tokens.next.kind in List(STATIC, IMPLICIT)) {
      val pos = tokens.next
      val modifier = tokens.next.kind match {
        case STATIC   =>
          eat(STATIC)
          Static()
        case IMPLICIT =>
          eat(IMPLICIT)
          Implicit()
        case _        => ???
      }
      modifiers += modifier.setPos(pos, tokens.lastVisible)
    }
    modifiers.toSet
  }

  /** <method> ::= <identifier> "(" [ <formal> { , <formal> } ] ")" [ : <returnType> ] <methodBody> */
  def method(modifiers: Set[Modifier]): MethodDecl = {
    modifiers.filterInstance[Implicit].foreach(mod => report(ImplicitMethodOrOperator(mod)))

    val id = identifier(MethodID)
    val args = commaList(LPAREN, RPAREN)(formal)
    val retType = optional(COLON)(returnType)

    val methBody = methodBody
    MethodDecl(id, modifiers, args, retType, methBody)
  }

  /** <constructor> ::= new "(" [ <formal> { , <formal> } ] ")" <methodBody> */
  def constructor(modifiers: Set[Modifier]): ConstructorDecl = {
    val pos = tokens.next
    eat(NEW)
    val methId = MethodID("new").setPos(pos, tokens.lastVisible)
    val args = commaList(LPAREN, RPAREN)(formal)
    val retType = Some(UnitType().setType(TUnit).setNoPos())
    val methBody = optional(EQSIGN)(statement)
    ConstructorDecl(methId, modifiers, args, retType, methBody)
  }

  /** <operator> ::= ( + | - | * | / | % | / | "|" | ^ | << | >> | < | <= | > | >= | ! | ~ | ++ | -- )
    * "(" <formal> [ , <formal> ] ")": <returnType> <methodBody>
    * */
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
      tokens.next.kind match {
        case COMMA =>
          eat(COMMA)
          val f2 = formal
          eat(RPAREN)
          if (tokens.next.kind == COMMA)
            eat(COMMA)
          val operatorType = Minus(Empty(), Empty()).setType(TUnit)
          (operatorType, List(f1, f2), modifiers + Static())
        case _     =>
          eat(RPAREN)
          val operatorType = Negation(Empty()).setType(TUnit)
          (operatorType, List(f1), modifiers + Static())
      }
    }

    def binaryOperator(constructor: (ExprTree, ExprTree) => OperatorTree): (OperatorTree, List[Formal], Set[Modifier]) = {
      eat(tokens.next.kind)
      val operatorType = constructor(Empty(), Empty()).setType(TUnit)
      eat(LPAREN)
      val f1 = formal
      eat(COMMA)
      val f2 = formal
      if (tokens.next.kind == COMMA)
        eat(COMMA)
      eat(RPAREN)
      (operatorType, List(f1, f2), modifiers + Static())
    }

    def unaryOperator(constructor: (ExprTree) => OperatorTree): (OperatorTree, List[Formal], Set[Modifier]) = {
      eat(tokens.next.kind)
      val operatorType: OperatorTree = constructor(Empty()).setType(TUnit)
      eat(LPAREN)
      val f = formal
      eat(RPAREN)
      (operatorType, List(f), modifiers + Static())
    }

    def indexingOperator = {
      eat(LBRACKET)

      val (numArgs, operatorType) = tokens.next.kind match {
        case RBRACKET =>
          eat(RBRACKET)
          tokens.next.kind match {
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
      if (args.lengthCompare(numArgs) != 0)
        report(UnexpectedToken(tokens.nextIncludingNewlines, tokens.last))

      (operatorType, args, modifiers)
    }

    val (operatorType, args, newModifiers) = tokens.next.kind match {
      case MINUS         => minusOperator
      case PLUS          => binaryOperator(Plus)
      case TIMES         => binaryOperator(Times)
      case DIV           => binaryOperator(Div)
      case MODULO        => binaryOperator(Modulo)
      case LOGICAND      => binaryOperator(LogicAnd)
      case LOGICOR       => binaryOperator(LogicOr)
      case LOGICXOR      => binaryOperator(LogicXor)
      case LSHIFT        => binaryOperator(LeftShift)
      case RSHIFT        => binaryOperator(RightShift)
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
        report(UnexpectedToken(tokens.next, tokens.last))
    }

    val retType = optional(COLON)(returnType)
    val methBody = methodBody

    OperatorDecl(operatorType.setNoPos(), newModifiers, args, retType, methBody)
  }

  /** <methodBody> ::= [ "=" <statement> ] */
  def methodBody: Option[StatTree] = optional(EQSIGN) { replaceWithReturnStatement(statement) }


  /** <formal> ::= <identifier> : <tpe> */
  def formal: Formal = positioned {
    val id = identifier(VariableID)
    eat(COLON)
    val typ = tpe
    Formal(typ, id)
  }

  private def replaceWithReturnStatement(stat: StatTree): StatTree = {
    val returnStatement = stat match {
      case Block(statements) if statements.nonEmpty =>
        val replaced = replaceWithReturnStatement(statements.last)
        Block(statements.updated(statements.size - 1, replaced))
      case acc@Access(_, _: MethodCall)             => Return(Some(acc))
      case UselessStatement(e)                      => Return(Some(e))
      case _                                        => stat
    }
    returnStatement.setPos(stat)
  }

  private def protectedOrPrivate: Accessability = positioned {
    tokens.next.kind match {
      case PROTECTED =>
        eat(PROTECTED)
        Protected()
      case _         => Private()
    }
  }


  //----------------------------------------------------------------------------------------------
  //--- Statements
  //----------------------------------------------------------------------------------------------


  /** <statement> ::=
    * | <varDeclaration>
    * | <block>
    * | <ifStatement>
    * | <returnStatement>
    * | <printlnStatement>
    * | <printStatement>
    * | <errorStatement>
    * | <forLoop>
    * | <whileLoop>
    * | <break>
    * | <continue>
    * | <emptyBlock>
    * | <expression>
    * */
  def statement: StatTree = tokens.next.kind match {
    case PRIVVAR | PRIVVAL => varDeclaration
    case INDENT            => block
    case IF                => ifStatement
    case RETURN            => returnStatement
    case PRINTLN           => printlnStatement
    case PRINT             => printStatement
    case ERROR             => errorStatement
    case FOR               => forLoop
    case WHILE             => whileLoop
    case BREAK             => break
    case CONTINUE          => continue
    case SEMICOLON         => emptyBlock
    case _                 => expression
  }

  /** <block> ::= <indent> { <statement> <statementEnd> } <dedent> */
  def block: Block = positioned {
    eat(INDENT)
    val statements = until(DEDENT) { statement after statementEnd }
    eat(DEDENT)
    Block(statements)
  }

  /** <ifStatement> ::= if"(" <expression> ")" <statement> [ else <statement> ] */
  def ifStatement: If = positioned {
    eat(IF, LPAREN)
    val condition = expression
    eat(RPAREN)
    val stmt = statement
    val els = optional(ELSE)(statement)
    If(condition, stmt, els)
  }

  /** <returnStatement> ::= return [ <expression> ] */
  def returnStatement: Return = positioned {
    eat(RETURN)
    tokens.nextIncludingNewlines.kind match {
      case SEMICOLON | NEWLINE | EOF => Return(None)
      case _                         => Return(Some(expression))
    }
  }

  /** <printStatement> ::= print"(" [ <expression> ] ")" */
  def printStatement: Print = printPrintlnError(PRINT, Print)

  /** <printlnStatement> ::= println"(" [ <expression> ] ")" */
  def printlnStatement: Println = printPrintlnError(PRINTLN, Println)

  /** <errorStatement> ::= error"(" [ <expression> ] ")" */
  def errorStatement: Error = printPrintlnError(ERROR, Error)

  private def printPrintlnError[T <: StatTree](methType: TokenKind, cons: ExprTree => T): T = positioned {
    eat(methType)
    eat(LPAREN)
    val expr = tokens.next.kind match {
      case RPAREN => StringLit("")
      case _      => expression
    }
    eat(RPAREN)

    cons(expr)
  }

  /** <forLoop> ::= for "(" <forInit> ";" [ <expression> ] ";" <forIncrement> ")" <statement> */
  def forLoop: StatTree = positioned {
    eat(FOR, LPAREN)

    tokens.next.kind match {
      case PRIVVAR | PRIVVAL =>
        val varDecl = varDeclaration
        if (varDecl.initiation.isDefined) {
          if (tokens.next.kind == COMMA)
            eat(COMMA)
          regularForLoop(Some(varDecl))
        } else {
          tokens.next.kind match {
            case IN =>
              forEachLoop(varDecl)
            case _  =>
              if (tokens.next.kind == COMMA)
                eat(COMMA)
              regularForLoop(Some(varDecl))
          }
        }
      case _                 =>
        regularForLoop(None)
    }
  }

  /** <regularForLoop> ::= <forInit> ";" [ <expression> ] ";" <forIncrement> ")" <statement> */
  def regularForLoop(firstVarDecl: Option[VarDecl]): For = {
    val init = forInit
    eat(SEMICOLON)
    val condition = tokens.next.kind match {
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

  /** <forEachLoop> ::= in <expression> ")" <statement> */
  def forEachLoop(varDecl: VarDecl): Foreach = {
    eat(IN)
    val container = expression
    eat(RPAREN)
    Foreach(varDecl, container, statement)
  }

  /** <forInit> ::= [ ( <assignment> | <varDeclaration> )  { "," ( <assignment> | <varDeclaration> ) } */
  def forInit: List[StatTree] =
    commaList(_ == SEMICOLON) {
      val startPos = tokens.next
      tokens.next.kind match {
        case PRIVVAR =>
          varDeclaration
        case _       =>
          val id = identifier(VariableID)

          val assignmentTokens = List(EQSIGN, PLUSEQ, MINUSEQ, DIVEQ, MODEQ, ANDEQ, OREQ, XOREQ, LSHIFTEQ, RSHIFTEQ)

          if (tokens.next.kind in assignmentTokens)
            assignment(Some(id)).asInstanceOf[Assign].setPos(startPos, tokens.lastVisible)
          else
            report(WrongToken(tokens.next, tokens.last, assignmentTokens.head, assignmentTokens.tail: _*))
      }
    }

  /** <whileLoop> ::= while"(" <expression> ")" <statement> */
  def whileLoop: While = positioned {
    eat(WHILE, LPAREN)
    val condition = expression
    eat(RPAREN)
    While(condition, statement)
  }

  /** <break> ::= break */
  def break: Break = simpleNode(BREAK, Break)

  /** <continue> ::= continue */
  def continue: Continue = simpleNode(CONTINUE, Continue)

  /** <emptyBlock> ::= ; */
  def emptyBlock: Block = positioned {
    eat(SEMICOLON)
    Block(Nil)
  }

  /** <statementEnd> ::= ( ; | \n ) { ; | \n } */
  def statementEnd()(implicit enclosing: Enclosing, line: Line): Unit = {
    debug"${ indentation }Ending statement"
    tokens.nextIncludingNewlines.kind match {
      case SEMICOLON | NEWLINE =>
        tokens.readNext()
        while (tokens.nextIncludingNewlines.kind in List(SEMICOLON, NEWLINE))
          tokens.readNext()
      case EOF                 =>
      case _                   => report(WrongToken(tokens.next, tokens.last, SEMICOLON, NEWLINE))
    }
  }


  //----------------------------------------------------------------------------------------------
  //--- Expressions
  //----------------------------------------------------------------------------------------------


  /** <expression> ::= <assignment> */
  def expression: ExprTree = assignment()

  /** <assignment> ::= <ternary> [ ( = | += | -= | *= | /= | %= | &= | |= | ^= | <<= | >>= ) <expression> ] **/
  def assignment(expr: Option[ExprTree] = None): ExprTree = {
    val startPos = tokens.next
    val e = expr getOrElse ternary

    def assignment(cons: (ExprTree, ExprTree) => ExprTree = null) = {
      val constructor = Option(cons)
      eat(tokens.next.kind)

      val value = expression
      val assignmentExpr = constructor
        .map(constructor => constructor(e, value).setPos(startPos, tokens.lastVisible))
        .getOrElse(value)

      e match {
        case a: Assignable => Assign(a, assignmentExpr).setPos(startPos, tokens.lastVisible)
        case _             => report(ExpectedIdAssignment(e))
      }
    }

    tokens.next.kind match {
      case EQSIGN   => assignment()
      case PLUSEQ   => assignment(Plus)
      case MINUSEQ  => assignment(Minus)
      case MULEQ    => assignment(Times)
      case DIVEQ    => assignment(Div)
      case MODEQ    => assignment(Modulo)
      case ANDEQ    => assignment(LogicAnd)
      case OREQ     => assignment(LogicOr)
      case XOREQ    => assignment(LogicXor)
      case LSHIFTEQ => assignment(LeftShift)
      case RSHIFTEQ => assignment(RightShift)
      case _        => e
    }
  }

  /** <ternary> ::= <elvis> [ ? <ternary> : <ternary> ] */
  def ternary: ExprTree = {
    val startPos = tokens.next
    val e = elvis
    if (tokens.next.kind == QUESTIONMARK) {
      eat(QUESTIONMARK)
      val thn = ternary
      eat(COLON)
      val els = ternary
      Ternary(e, thn, els).setPos(startPos, tokens.lastVisible)
    } else {
      e
    }
  }

  /** <elvis> ::= <or> [ ?: <elvis> ] */
  def elvis: ExprTree = {
    val startPos = tokens.next
    val e = or
    if (tokens.next.kind == ELVIS) {
      eat(ELVIS)
      Elvis(e, elvis).setPos(startPos, tokens.lastVisible)
    } else {
      e
    }
  }

  /** <or> ::= <and> { || <and> } */
  def or: ExprTree = leftAssociativeOperator(OR)(and)

  /** <and> ::= <eqNotEq> { && <eqNotEq> } */
  def and: ExprTree = leftAssociativeOperator(AND)(eqNotEq)

  /** <eqNotEq> ::= <is> { ( == | != ) <is> } */
  def eqNotEq: ExprTree = leftAssociativeOperator(EQUALS, NOTEQUALS)(is)

  /** <is> ::= <as> { is <tpe> } */
  def is: ExprTree = {
    val startPos = tokens.next
    var e = as
    while (tokens.nextIncludingNewlines.kind == IS) {
      eat(IS)
      e = Is(e, tpe).setPos(startPos, tokens.lastVisible)
    }
    e
  }

  /** <as> ::= <comparison> { as <tpe> } */
  def as: ExprTree = {
    val startPos = tokens.next
    var e = comparison
    while (tokens.nextIncludingNewlines.kind == AS) {
      eat(AS)
      e = As(e, tpe).setPos(startPos, tokens.lastVisible)
    }
    e
  }

  /** <comparison> ::= <logicOr> { ( < | <= | > | >= | inst ) <logicOr> } */
  def comparison: ExprTree = leftAssociativeOperator(LESSTHAN, LESSTHANEQ, GREATERTHAN, GREATERTHANEQ)(logicOr)

  /** <logicOr> ::= <logicXor> { | <logicXor> } */
  def logicOr: ExprTree = leftAssociativeOperator(LOGICOR)(logicXor)

  /** <logicXor> ::= <logicAnd> { ^ <logicAnd> } */
  def logicXor: ExprTree = leftAssociativeOperator(LOGICXOR)(logicAnd)

  /** <logicAnd> ::= <bitShift> { & <bitShift> } */
  def logicAnd: ExprTree = leftAssociativeOperator(LOGICAND)(bitShift)

  /** <bitShift> ::= <plusMinus> { ( << | >> ) <plusMinus> } */
  def bitShift: ExprTree = leftAssociativeOperator(LSHIFT, RSHIFT)(plusMinus)

  /** <plusMinus> ::= <timesDiv> { ( + | - ) <timesDiv> } */
  def plusMinus: ExprTree = leftAssociativeOperator(PLUS, MINUS)(timesDivMod)

  /** <timesDivMod> ::= <term> { ( * | / | % ) <term> } */
  def timesDivMod: ExprTree = leftAssociativeOperator(TIMES, DIV, MODULO)(term)

  /** <term> ::= <termFirst> <termRest> */
  def term: ExprTree = termRest(termFirst)

  /** <termFirst> ::=
    * | <identifierOrMethodCall>
    * | <intLit>
    * | <stringLit>
    * | <charLit>
    * | <longLit>
    * | <doubleLit>
    * | <floatLit>
    * | <newExpression>
    * | <paren>
    * | <arrayLit>
    * | <nullTerm>
    * | <thisTerm>
    * | <trueTerm>
    * | <falseTerm>
    * | <notTerm>
    * | <logicNot>
    * | <hash>
    * | <increment>
    * | <decrement>
    * | <negation>
    * | <superCall>
    */
  def termFirst: ExprTree = {
    // These are sorted by commonness. For instance IDKIND and INTLIT are the by
    // far most common terms, thats why they appear first.
    tokens.next.kind match {
      case IDKIND        => identifierOrMethodCall
      case INTLITKIND    => literal(INTLITKIND, IntLit)
      case STRLITKIND    => literal(STRLITKIND, StringLit)
      case CHARLITKIND   => literal(CHARLITKIND, CharLit)
      case LONGLITKIND   => literal(LONGLITKIND, LongLit)
      case DOUBLELITKIND => literal(DOUBLELITKIND, DoubleLit)
      case FLOATLITKIND  => literal(FLOATLITKIND, FloatLit)
      case NEW           => newExpression
      case LPAREN        => paren
      case LBRACKET      => arrayLit
      case NULL          => nullTerm
      case THIS          => thisTerm
      case TRUE          => trueTerm
      case FALSE         => falseTerm
      case BANG          => notTerm
      case LOGICNOT      => logicNot
      case HASH          => hash
      case INCREMENT     => preIncrement
      case DECREMENT     => preDecrement
      case MINUS         => negation
      case SUPER         => superCall
      case _             => report(UnexpectedToken(tokens.next, tokens.last))
    }
  }

  /** <identifierOrMethodCall> ::= <identifier> { :: <identifier> } [ "(" <expression> { , <expression> } ")" ] */
  def identifierOrMethodCall: ExprTree = positioned {
    val methStartPos = tokens.next
    val ids = nonEmptyList(COLON, COLON)(identifierName)
    val name = ids.mkString("::")
    tokens.next.kind match {
      case LPAREN =>
        val id = MethodID(name).setPos(methStartPos, tokens.lastVisible)
        val exprs = commaList(LPAREN, RPAREN)(expression)
        val meth = MethodCall(id, exprs).setPos(methStartPos, tokens.lastVisible)
        NormalAccess(Empty(), meth)
      case _      => VariableID(name)
    }
  }

  /** <newExpression> ::= new <classType> [ "?" <nullableBracket> ] { <nullableBracket> } */
  def newExpression: ExprTree = positioned {
    val startPos = tokens.next
    eat(NEW)
    val tpe: TypeTree = classType

    tokens.next.kind match {
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
        if (tokens.next.kind == QUESTIONMARK) {
          eat(QUESTIONMARK)
          e = NullableType(e).setPos(startPos, tokens.lastVisible)
          _nullableBracket()
        }

        while (tokens.next.kind == LBRACKET)
          _nullableBracket()

        NewArray(e, sizes.toList)
      case _                       => report(WrongToken(tokens.next, tokens.last, LPAREN, LBRACKET))
    }
  }

  /** <nullableBracket> ::=  "[" <expression> "]" [ ? ] */
  def nullableBracket(startPos: Positioned, tpe: TypeTree): (TypeTree, ExprTree) = {
    var e = tpe
    eat(LBRACKET)
    val size = expression
    eat(RBRACKET)
    e = ArrayType(e).setPos(startPos, tokens.lastVisible)
    if (tokens.next.kind == QUESTIONMARK) {
      eat(QUESTIONMARK)
      e = NullableType(e).setPos(startPos, tokens.lastVisible)
    }
    (e, size)
  }

  /** <paren> ::= "(" <expression> ") */
  def paren: ExprTree = positioned {
    eat(LPAREN)
    val expr = expression
    eat(RPAREN)
    expr
  }

  /** <arrayLit> ::= "[" <expression> { , <expression> } "]" */
  def arrayLit: ArrayLit = positioned {
    val expressions = commaList(LBRACKET, RBRACKET)(expression)
    ArrayLit(expressions)
  }

  /** <nullTerm> ::= null */
  def nullTerm: NullLit = simpleNode(NULL, NullLit)

  /** <thisTerm> ::= this */
  def thisTerm: This = simpleNode(THIS, This)

  /** <trueTerm> ::= true */
  def trueTerm: TrueLit = simpleNode(TRUE, TrueLit)

  /** <falseTerm> ::= false */
  def falseTerm: FalseLit = simpleNode(FALSE, FalseLit)

  /** <notTerm> ::= ! <term> */
  def notTerm: Not = unaryOperatorTerm(BANG, Not)

  /** <logicNot> ::= ~ <term> */
  def logicNot: LogicNot = unaryOperatorTerm(LOGICNOT, LogicNot)

  /** <hash> ::= # <term> */
  def hash: Hash = unaryOperatorTerm(HASH, Hash)

  /** <preIncrement> ::= ++ <term> */
  def preIncrement: PreIncrement = unaryOperatorTerm(INCREMENT, PreIncrement)

  /** <preDecrement> ::= -- <term> */
  def preDecrement: PreDecrement = unaryOperatorTerm(DECREMENT, PreDecrement)

  /** <negation> ::= - <term> */
  def negation: ExprTree = positioned {
    eat(MINUS)
    tokens.next match {
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

  /** <superCall> ::=  <superTerm> . <application> */
  def superCall: Access = positioned {
    val term = superTerm
    eat(DOT)
    NormalAccess(term, application)
  }

  /** <superTerm> ::= super [ "<" <identifier> "> ] */
  def superTerm: ExprTree = positioned {
    eat(SUPER)
    val specifier = optional(LESSTHAN) { identifier(ClassID(_, Nil)) after eat(GREATERTHAN) }
    Super(specifier)
  }

  /** <access> ::= (. | ?.) <identifier> [ "(" <expression> { , <expression> } ")" ] */
  def access(obj: ExprTree): Access = positioned {
    val access = tokens.next.kind match {
      case DOT        =>
        eat(DOT)
        NormalAccess
      case SAFEACCESS =>
        eat(SAFEACCESS)
        SafeAccess
      case _          => report(WrongToken(tokens.next, tokens.last, DOT, QUESTIONMARK))
    }

    access(obj, application)
  }

  /** <application> ::= <identifier> [ "(" <expression> { , <expression> } ")" ] */
  def application: ExprTree = positioned {
    val id = identifier(VariableID)
    tokens.next.kind match {
      case LPAREN =>
        val exprs = commaList(LPAREN, RPAREN)(expression)
        val methId = MethodID(id.name).setPos(id)
        MethodCall(methId, exprs)
      case _      => id
    }
  }

  /** <termRest> ::=  { <access> | <arrayIndexing> | ++ | -- | !! } */
  def termRest(termFirst: ExprTree): ExprTree = {
    var e = termFirst
    // Uses current token since a newline should stop the iteration
    while (tokens.nextIncludingNewlines.kind in List(DOT, SAFEACCESS, EXTRACTNULLABLE, LBRACKET, INCREMENT, DECREMENT)) {
      e = tokens.nextIncludingNewlines.kind match {
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
        case EXTRACTNULLABLE  =>
          eat(EXTRACTNULLABLE)
          ExtractNullable(e)
        case _                => ???
      }
      e.setPos(termFirst, tokens.lastVisible)
    }
    e
  }

  /** <arrayIndexing> ::= "[" [ <expression> ] [ : [ <expression> ] ] [ : [ <expression> ] ] "]" */
  def arrayIndexing(arr: ExprTree): ExprTree = positioned {
    eat(LBRACKET)
    val exprs = until(RBRACKET) {
      tokens.next.kind match {
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


  //----------------------------------------------------------------------------------------------
  //--- Misc trees, literals, identifiers, types
  //----------------------------------------------------------------------------------------------

  /** <tpe> ::= <classType> { "[]" | ? } */
  def tpe: TypeTree = {
    val startPos = tokens.next
    var e: TypeTree = classType
    var dimension = 0

    while (tokens.next.kind in List(QUESTIONMARK, LBRACKET)) {
      e = tokens.next.kind match {
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

  /** <returnType> ::= Unit | <tpe> */
  def returnType: TypeTree = positioned {
    tpe match {
      case ClassID("Unit", _) => UnitType()
      case t                  => t
    }
  }

  /** <simpleNode> ::= <kind> */
  private def simpleNode[T <: Tree](kind: TokenKind, tree: () => T): T = positioned {
    eat(kind)
    tree()
  }

  /** <unaryOperatorTerm> ::= <kind> <term> */
  private def unaryOperatorTerm[T <: ExprTree](kind: TokenKind, operator: ExprTree => T) = positioned {
    eat(kind)
    operator(term)
  }

  /** <classTypeIdentifier> ::= <identifier> [ "<" <identifier> { "," <identifier> } ">" ] */
  def classTypeIdentifier: ClassID = positioned {
    tokens.next match {
      case id: ID =>
        eat(IDKIND)
        val templateIds = tokens.next.kind match {
          case LESSTHAN => commaList(LESSTHAN, GREATERTHAN)(identifier(ClassID(_, Nil)))
          case _        => List()
        }
        ClassID(id.value, templateIds)
      case _      => report(WrongToken(tokens.next, tokens.last, IDKIND))
    }
  }

  /** <classType> ::= <identifier> { :: <identifier> } <templateList> */
  def classType: ClassID = positioned {
    val ids = nonEmptyList(COLON, COLON)(identifierName)
    val id = ids.mkString("::")
    ClassID(id, templateList)
  }

  /** <templateList> ::= [ "<" <type> { "," <type> } ">" ] */
  private def templateList: List[TypeTree] = {
    tokens.next.kind match {
      case LESSTHAN =>
        eat(LESSTHAN)
        commaList(stop = _ in List(GREATERTHAN, RSHIFT))(tpe) after endTemplateList
      case _        => List()
    }
  }

  /**
    * Handles the conflict of generics having multiple ">" signs by
    * treating RSHIFT (>>) as two ">".
    */
  private var toSpare = 0
  private def endTemplateList(): Unit = {
    tokens.next.kind match {
      case GREATERTHAN =>
        eat(GREATERTHAN)
      case RSHIFT      =>
        eat(RSHIFT)
        toSpare += 1
      case _           =>
        toSpare -= 1
        if (toSpare < 0)
          report(WrongToken(tokens.next, tokens.last, GREATERTHAN))
    }
  }

  /** <identifier> ::= sequence of letters, digits and underscores, starting with a letter and which is not a keyword */
  private def identifier[T <: Identifier[_]](tpe: String => T): T = positioned { tpe(identifierName) }

  /** Parses the name of the identifier without creating an identifier object */
  private def identifierName: String = tokens.next match {
    case id: ID =>
      eat(IDKIND)
      id.value
    case _      => report(WrongToken(tokens.next, tokens.last, IDKIND))
  }

  /** Parses a literal of the given kind, producing the given literalType
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


  //----------------------------------------------------------------------------------------------
  //--- Help functions
  //----------------------------------------------------------------------------------------------


  /** Eats the expected tokens, or terminates with an error.
    * Takes enclosing and line as implicit parameters so that the logging
    * statement will point to the calling method instead of here.
    * */
  private def eat(kinds: TokenKind*)(implicit enclosing: Enclosing, line: Line): Unit = {
    val numNewlines = tokens.readNewLines()
    debug"${ indentation }Eating tokens ${ (List.fill(numNewlines)(NEWLINE) ::: kinds.toList).mkString(", ") }."

    for (k <- kinds) tokens.next.kind match {
      case `k` => tokens.readNext()
      case _   => report(WrongToken(tokens.next, tokens.last, k))
    }

    trace"${ indentation }Tokens left: ${ NL + tokens.toString }"
  }

  /** <leftAssociativeOperator> ::= <next> { ( kinds[0] | kinds[1] | ... | kinds[n] ) <next> } */
  private def leftAssociativeOperator(kinds: TokenKind*)(next: => ExprTree): ExprTree = {
    val startPos = tokens.next

    def matchingKind = kinds.find(_ == tokens.nextIncludingNewlines.kind)

    var expr = next
    var kind = matchingKind
    while (kind.isDefined) {
      eat(kind.get)
      expr = TokenToBinaryOperatorAST(kind.get)(expr, next).setPos(startPos, tokens.lastVisible)
      kind = matchingKind
    }
    expr
  }

  /** <nonEmptyList> ::= <parse> { <delimiter_0> ... <delimiter_n> <parse> } */
  private def nonEmptyList[T](delimiters: TokenKind*)(parse: => T): List[T] = {
    val b = ListBuffer[T]()
    b += parse

    def matches = (0 until delimiters.size).forall(i => tokens.offset(i).kind == delimiters(i))

    while (matches) {
      delimiters.foreach(eat(_))
      b += parse
    }
    b.toList
  }

  /** * <commaList> ::= <startToken> [ <parse> { "," <parse> } ] <endToken> */
  def commaList[T](stop: TokenKind => Boolean)(parse: => T): List[T] = {
    if (stop(tokens.next.kind)) {
      return List()
    }

    val b = ListBuffer[T](parse)
    while (tokens.next.kind == COMMA) {
      eat(COMMA)

      // To allow for trailing commas
      if (!stop(tokens.next.kind))
        b += parse
    }

    b.toList
  }

  def commaList[T](startToken: TokenKind, stopToken: TokenKind)(parse: => T): List[T] = {
    eat(startToken)
    val items = commaList(_ == stopToken)(parse)
    eat(stopToken)
    items
  }


  /** <optional> ::= [ <parse> ] */
  private def optional[T <: Positioned](kinds: TokenKind*)(parse: => T): Option[T] = {
    if (tokens.next.kind notIn kinds)
      return None

    eat(tokens.next.kind)
    Some(parse)
  }

  // Continues parsing until one of the given token kinds are encountered
  private def until[T](kinds: TokenKind*)(parse: => T): List[T] = until(tokens.next.kind notIn kinds)(parse)

  // Continues parsing until a token different from the given tokens is encountered
  private def untilNot[T](kinds: TokenKind*)(parse: => T): List[T] = until(tokens.next.kind in kinds)(parse)

  // Continues parsing while the condition is met
  private def until[T](condition: => Boolean)(parse: => T): List[T] = {
    var b = ListBuffer[T]()
    while (condition) b += parse
    b.toList
  }

  // Executes the given function and sets the start and end position on the resulting value
  private def positioned[T <: Positioned](parse: => T)(implicit enclosing: Enclosing, line: Line): T = {
    debug"${ indentation }Parsing ${ enclosing.method }"

    indent += 1
    val startPos = tokens.next
    val res = parse
    indent -= 1
    res.setPos(startPos, tokens.lastVisible)
  }

  private def indentation: String = {
    import ctx.formatter.formatting._
    VerticalRight + Horizontal * indent + " "
  }

}
