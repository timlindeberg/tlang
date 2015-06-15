package koolc
package ast

import java.beans.Expression

import utils._
import Trees._
import lexer._
import lexer.Tokens._
import scala.collection.mutable.ArrayBuffer

object Parser extends Pipeline[Iterator[Token], Program] {

  var currentToken: Token = new Token(BAD)

  def run(ctx: Context)(tokens: Iterator[Token]): Program = {
    val astBuilder = new ASTBuilder(ctx, tokens)
    astBuilder.readToken
    astBuilder.parseGoal
  }

  private class ASTBuilder(ctx: Context, tokens: Iterator[Token]) {

    import ctx.reporter._
    /** Store the current token, as read from the lexer. */

    def readToken: Unit = {
      if (tokens.hasNext) {
        // uses nextToken from the Lexer trait
        currentToken = tokens.next

        // skips bad tokens
        while (currentToken.kind == BAD) {
          currentToken = tokens.next
        }
      }
    }

    /** ''Eats'' the expected token, or terminates with an error. */
    private def eat(kind: TokenKind*): Unit =
      for (k <- kind) {
        if (currentToken.kind == k) {
          readToken
        } else {
          expected(k)
        }
      }

    /** Complains that what was found was not expected. The method accepts arbitrarily many arguments of type TokenKind */
    private def expected(kind: TokenKind, more: TokenKind*): Nothing =
      fatal("expected: " + (kind :: more.toList).mkString(" or ") + ", found: " + currentToken, currentToken)

    /**
     * <goal> ::= <mainObject> { <classDeclaration> } <EOF>
     */
    def parseGoal() = {
      val pos = currentToken
      Program(mainObject, until(classDeclaration, EOF)).setPos(pos)
    }

    /**
     * <mainObject> ::= object <identifier> "{" def main (): Unit = "{" { <statement> } "}" "}"
     */
    private def mainObject(): MainObject = {
      val pos = currentToken
      eat(OBJECT)
      val id = identifier
      eat(LBRACE, DEF, MAIN, LPAREN, RPAREN, COLON, UNIT, EQSIGN, LBRACE)
      val stmts = until(statement, RBRACE)
      eat(RBRACE, RBRACE)
      MainObject(id, stmts).setPos(pos)
    }

    /**
     * <classDeclaration> ::= class <typeIdentifier>
     *    [ extends <typeIdentifier> ] "{" { <varDeclaration> } { methodDeclaration } "}"
     */
    private def classDeclaration(): ClassDecl = {
      val pos = currentToken
      eat(CLASS)
      val id = classTypeIdentifier
      val parent = optional(typeIdentifier, EXTENDS)
      eat(LBRACE)
      val vars = untilNot(varDeclaration, VAR)
      val methods = untilNot(methodDeclaration, DEF)
      eat(RBRACE)
      ClassDecl(id, parent, vars, methods).setPos(pos)
    }

    /**
     * <varDeclaration> ::= var <identifier> ":" <tpe> ";"
     */
    private def varDeclaration(): VarDecl = {
      val pos = currentToken
      eat(VAR)
      val id = identifier
      eat(COLON)
      val typ = tpe
      eat(SEMICOLON)
      VarDecl(typ, id).setPos(pos)
    }

    /**
     * <formal> ::= <identifier> ":" <tpe>
     */
    private def formal(): Formal = {
      val pos = currentToken
      val id = identifier
      eat(COLON)
      val typ = tpe
      Formal(typ, id).setPos(pos)
    }

    /**-
     * <methodDeclaration> ::= def <identifier> "(" [ <formal> { "," <formal> } ] "): " (<tpe> | Unit) "= {" { <varDeclaration> } { <statement> } "}"
     *                       | def <identifier> "(" [ <formal> { "," <formal> } ] ") = {" { <varDeclaration> } { <statement> } "}"
     */
    private def methodDeclaration(): FuncTree = {
      val pos = currentToken
      eat(DEF)
      val id = identifier
      eat(LPAREN)
      val args = commaList(formal)

      eat(RPAREN)
      val func = if(currentToken.kind == COLON){
        eat(COLON)
        val retType = if(currentToken.kind == UNIT){
          val pos = currentToken
          eat(UNIT)
          UnitType().setPos(pos)
        }else{
          tpe
        }
        eat(EQSIGN, LBRACE)
        val vars = untilNot(varDeclaration, VAR)
        val stmts = until(statement, RBRACE)
        eat(RBRACE)
        MethodDecl(retType, id, args, vars, stmts)
      }else{
        eat(EQSIGN, LBRACE)
        val vars = untilNot(varDeclaration, VAR)
        val stmts = until(statement, RBRACE)
        eat(RBRACE)
        ConstructorDecl(id, args, vars, stmts)
      }
      func.setPos(pos)
    }

    /**
     * <tpe> ::= ( Int | Bool | String | <typeIdentifier> ) { "[]" }
     */
    private def tpe(): TypeTree = {
      val pos = currentToken
      val tpe = currentToken.kind match {
        case INT => {
          eat(INT)
          IntType()
        }
        case BOOLEAN =>
          eat(BOOLEAN)
          BooleanType()
        case STRING =>
          eat(STRING)
          StringType()
        case _ => typeIdentifier
      }
      var e = tpe
      while(currentToken.kind == LBRACKET){
        e.setPos(pos)
        eat(LBRACKET, RBRACKET)
        e = ArrayType(e)
      }
      e.setPos(pos)
    }

    /**
     * <statement> ::= "{" { <statement> } "}
     *               | if"(" <expression> ")" <statement> [ else <statement> ]
     *               | while"(" <expression> ")" <statement>
     *               | for "("[ <assignment> { "," <assignment> } ";" <expression> ";" [ <expression> { "," <expression> } ] " ) <statement>
     *               | println"(" <expression> ");"
     *               | return [ <expression> ] ";"
     *               | <identifier> "=" <expression> ";"
     *               | <identifier> "+=" <expression> ";"
     *               | <identifier> "-=" <expression> ";"
     *               | <identifier> "*=" <expression> ";"
     *               | <identifier> "/=" <expression> ";"
     *               | <identifier> "%=" <expression> ";"
     *               | <identifier> "&=" <expression> ";"
     *               | <identifier> "|=" <expression> ";"
     *               | <identifier> "^=" <expression> ";"
     *               | <identifier> "<<=" <expression> ";"
     *               | <identifier> ">>=" <expression> ";"
     *               | <identifier> "[" <expression> "]" "=" <expression> ";"
     *               | <identifier> "++"
     *               | <identifier> "--"
     *               | "++" <identifier>
     *               | "--" <identifier>
     *               | <expression>"."<identifier>"(" [ <expression> { "," <expression> } ] [ "."<identifier>"(" [ <expression> { "," <expression> } ] }
     */
    private def statement(): StatTree = {
      val pos = currentToken
      val tree = currentToken.kind match {
        case LBRACE =>
          eat(LBRACE)
          val stmts = until(statement, RBRACE)
          eat(RBRACE)
          Block(stmts)
        case IF =>
          eat(IF, LPAREN)
          val expr = expression
          eat(RPAREN)
          val stmt = statement
          val els = optional(statement, ELSE)
          If(expr, stmt, els)
        case WHILE =>
          eat(WHILE, LPAREN)
          val expr = expression
          eat(RPAREN)
          While(expr, statement)
        case FOR =>
          eat(FOR, LPAREN)
          val init = commaList( () => {
            val id = identifier
            eat(EQSIGN)
            val expr = expression
            Assign(id, expr)
          }, SEMICOLON)
          eat(SEMICOLON)
          val condition = expression
          eat(SEMICOLON)
          val post = commaList(() => currentToken.kind match {
            case INCREMENT =>
              eat(INCREMENT)
              PreIncrement(identifier)
            case DECREMENT =>
              eat(DECREMENT)
              PreDecrement(identifier)
            case IDKIND =>
              val id = identifier
              currentToken.kind match {
                case INCREMENT =>
                  eat(INCREMENT)
                  PostIncrement(id)
                case DECREMENT =>
                  eat(DECREMENT)
                  PostDecrement(id)
                case _ => expected(INCREMENT, DECREMENT)
              }
            case _ => expected(INCREMENT, DECREMENT, IDKIND)
          })
          eat(RPAREN)
          For(init, condition, post, statement)
        case PRINTLN =>
          eat(PRINTLN, LPAREN)
          val expr = expression
          eat(RPAREN, SEMICOLON)
          Println(expr)
        case RETURN =>
          eat(RETURN)
          val expr = if(currentToken.kind != SEMICOLON) Some(expression) else None
          eat(SEMICOLON)
          Return(expr)
        case IDKIND =>
          val id = identifier
          def assignment(constructor: (Identifier, ExprTree) => StatTree) = {
            eat(currentToken.kind)
            constructor(id, expression)
          }
          val stat = currentToken.kind match {
            case EQSIGN       => assignment(Assign)
            case PLUSEQ       => assignment(PlusAssign)
            case MINUSEQ      => assignment(MinusAssign)
            case MULEQ        => assignment(MulAssign)
            case DIVEQ        => assignment(DivAssign)
            case MODEQ        => assignment(ModAssign)
            case ANDEQ        => assignment(AndAssign)
            case OREQ         => assignment(OrAssign)
            case XOREQ        => assignment(XorAssign)
            case LEFTSHIFTEQ  => assignment(LeftShiftAssign)
            case RIGHTSHIFTEQ => assignment(RightShiftAssign)
            case LBRACKET =>
              eat(LBRACKET)
              val expr1 = expression
              eat(RBRACKET, EQSIGN)
              val expr2 = expression
              ArrayAssign(id, expr1, expr2)
            case DOT =>
              var e: ExprTree = id
              while(currentToken.kind == DOT){
                eat(DOT)
                val methName = identifier
                eat(LPAREN)
                val args = commaList(expression)
                eat(RPAREN)
                e = MethodCall(e, methName, args)
              }
              e.asInstanceOf[MethodCall]
            case INCREMENT =>
              eat(INCREMENT)
              PostIncrement(id)
            case DECREMENT =>
              eat(DECREMENT)
              PostDecrement(id)
            case LPAREN =>
              eat(LPAREN)
              val exprs = commaList(expression)
              eat(RPAREN)
              MethodCall(This(), id, exprs) // Implicit this
            case _ => expected(EQSIGN, LBRACKET)
          }
          eat(SEMICOLON)
          stat
        case _ =>
          val expr = expression
          eat(SEMICOLON)
          expr match {
            case m : MethodCall    => m
            case m : PreIncrement  => m
            case m : PreDecrement  => m
            case _                 => fatal("Not a valid statement, expected println, if, while, assignment, a method call or incrementation/decrementation. ", expr)
          }
      }
      tree.setPos(pos)
    }

    /**
     * <expression> ::= <ternary>
     */
    private def expression(): ExprTree = {
      val pos = currentToken
      val exprMap: Map[TokenKind, (ExprTree, ExprTree) => ExprTree] = Map(
        OR                -> Or,
        AND               -> And,
        LESSTHAN          -> LessThan,
        LESSTHANEQUALS    -> LessThanEquals,
        GREATERTHAN       -> GreaterThan,
        GREATERTHANEQUALS -> GreaterThanEquals,
        EQUALS            -> Equals,
        NOTEQUALS         -> NotEquals,
        PLUS              -> Plus,
        MINUS             -> Minus,
        TIMES             -> Times,
        DIV               -> Div,
        MODULO            -> Modulo,
        LEFTSHIFT         -> LeftShift,
        RIGHTSHIFT        -> RightShift,
        LOGICAND          -> LogicAnd,
        LOGICOR           -> LogicOr,
        LOGICXOR          -> LogicXor)

      /**
       * Parses expressions of type
       * E ::= <next> { ( kinds[0] | kinds[1] | ... | kinds[n] ) <next> }.
       * Used to parse left associative expressions. *
       */
      def left(next: () => ExprTree, kinds: TokenKind*): ExprTree = {
        var expr = next()
        while (kinds.contains(currentToken.kind)) {
          kinds.foreach { kind =>
            if (currentToken.kind == kind) {
              val pos = currentToken
              eat(kind)
              expr = exprMap(kind)(expr, next()).setPos(pos)
            }
          }
        }
        expr
      }

      /** <ternary> ::= <or> [ ? <or> : <or> ]*/
      def ternary() = {
        var e = or
        if(currentToken.kind == QUESTIONMARK){
          eat(QUESTIONMARK)
          val thn = or
          eat(COLON)
          val els = or
          e = Ternary(e, thn, els)
        }
        e
      }

      /** <or> ::= <and> { || <and> } */
      def or() = left(and, OR)

      /** <and> ::= <logicOr> { && <logicOr> } */
      def and() = left(logicOr, AND)

      /** <logicOr> ::= <logicXor> { | <logicXor> } */
      def logicOr() = left(logicXor, LOGICOR)

      /** <logicXor> ::= <logicAnd> { ^ <logicAnd> } */
      def logicXor() = left(logicAnd, LOGICXOR)

      /** <logicAnd> ::= <eqNotEq> { & <eqNotEq> } */
      def logicAnd() = left(eqNotEq, LOGICAND)

      /** <eqNotEq> ::= <instOf> { ( == | != ) <instOf> } */
      def eqNotEq() = left(instOf, EQUALS, NOTEQUALS)

      /** <instOf> ::= <comparison> { inst <identifier> } */
      def instOf() = {
        var e = comparison
        while(currentToken.kind == INSTANCEOF){
          eat(INSTANCEOF)
          e = Instance(e, identifier)
        }
        e
      }

      /** <comparison> ::= <bitShift> { ( < | <= | > | >= | inst ) <bitShift> } */
      def comparison() = left(bitShift, LESSTHAN, LESSTHANEQUALS, GREATERTHAN, GREATERTHANEQUALS)

      /** <bitShift> ::= <plusMinus> { ( << | >> ) <plusMinus> } */
      def bitShift() = left(plusMinus, LEFTSHIFT, RIGHTSHIFT)

      /** <plusMinus> ::= <timesDiv> { ( + | - ) <timesDiv> } */
      def plusMinus() = left(timesDivMod, PLUS, MINUS)

      /** <timesDivMod> ::= <term> { ( * | / | % ) <term> } */
      def timesDivMod() = left(term, TIMES, DIV, MODULO)

      /**
       * <term> ::= <termFirst> [ termRest ]
       */
      def term(): ExprTree = {
        /**
         * <termFirst> ::= "(" <expression> ")"
         *               | ! <expression>
         *               | - <expression>
         *               | -- <identifier>
         *               | ++ <identifier>
         *               | ~ <identifier>
         *               | <intLit>
         *               | <stringLit>
         *               | <identifier>
         *               | <identifier> ++
         *               | <identifier> --
         *               | <identifier> "(" <expression> { "," <expression> } ")"
         *               | true
         *               | false
         *               | this
         *               | new <tpe>"[" <expression> "]"
         *               | new <typeIdentifier> "(" [ <expression> { "," <expression> } ")"
         */
        def termFirst() = {
          val tree = currentToken.kind match {
            case LPAREN =>
              eat(LPAREN)
              val expr = expression
              eat(RPAREN)
              expr
            case BANG =>
              eat(BANG)
              Not(term)
            case MINUS =>
              eat(MINUS)
              Negation(term)
            case LOGICNOT =>
              eat(LOGICNOT)
              LogicNot(term)
            case DECREMENT =>
              eat(DECREMENT)
              PreDecrement(identifier)
            case INCREMENT =>
              eat(INCREMENT)
              PreIncrement(identifier)
            case INTLITKIND =>
              intLit
            case STRLITKIND =>
              stringLit
            case IDKIND =>
              val id = identifier
              currentToken.kind match {
                case INCREMENT =>
                  eat(INCREMENT)
                  PostIncrement(id)
                case DECREMENT =>
                  eat(DECREMENT)
                  PostDecrement(id)
                case LPAREN =>
                  eat(LPAREN)
                  val exprs = commaList(expression)
                  eat(RPAREN)
                  MethodCall(This(), id, exprs) // Implicit this
                case _ => id
              }
            case TRUE =>
              eat(TRUE)
              True()
            case FALSE =>
              eat(FALSE)
              False()
            case THIS =>
              eat(THIS)
              This()
            case NEW =>
              eat(NEW)
              def primitiveArray(construct: () => TypeTree) = {
                eat(currentToken.kind, LBRACKET)
                val expr = expression
                eat(RBRACKET)
                NewArray(construct(), expr)
              }
              currentToken.kind match {
                case INT     => primitiveArray(IntType)
                case STRING  => primitiveArray(StringType)
                case BOOLEAN => primitiveArray(BooleanType)
                case _ =>
                  val id = typeIdentifier
                  currentToken.kind match {
                    case LPAREN =>
                      eat(LPAREN)
                      val args = commaList(expression)
                      eat(RPAREN)
                      New(id, args)
                    case LBRACKET =>
                      eat(LBRACKET)
                      val expr = expression
                      eat(RBRACKET)
                      NewArray(id, expr)
                    case _ => expected(LPAREN, LBRACKET)
                  }

              }
            case _ => expected(LPAREN, BANG, INTLITKIND, STRLITKIND, IDKIND, TRUE, FALSE, THIS, NEW)
          }
          tree.setPos(pos)
        }

        /**
         * <termRest> ::= .length
         *              | .<identifier> "(" <expression> { "," <expression> } ")
         *              | "[" <expression> "]"
         *              | as <tpe>
         */
        def termRest(lhs: ExprTree): ExprTree = {
          val pos = currentToken
          var e = lhs
          val tokens = List(DOT, LBRACKET, AS)

          while (tokens.contains(currentToken.kind)) {
            e = currentToken.kind match {
              case DOT =>
                eat(DOT)
                if (currentToken.kind == LENGTH) {
                  eat(LENGTH)
                  ArrayLength(e)
                } else {
                  val id = identifier
                  eat(LPAREN)
                  val exprs = commaList(expression)
                  eat(RPAREN)
                  MethodCall(e, id, exprs.toList)
                }
              case LBRACKET =>
                eat(LBRACKET)
                val expr = expression
                eat(RBRACKET)
                ArrayRead(e, expr)
              case AS =>
                eat(AS)
                As(e, tpe)
              case _ => e
            }
          }

          e.setPos(pos)
        }
        termRest(termFirst)
      }
      ternary().setPos(pos)
    }

    private var usedOneGreaterThan = false

    /**
     * Handles the conflict of generics having multiple ">" signs by
     * treating RIGHTSHIFT (>>) as two ">".
     */
    private def eatRightShiftOrGreaterThan() =
      if(currentToken.kind == RIGHTSHIFT){
        if(usedOneGreaterThan){
          eat(RIGHTSHIFT)
        }
        usedOneGreaterThan = !usedOneGreaterThan
      }else{
        eat(GREATERTHAN)
      }

    /**
     * <classTypeIdentifier> ::= <identifier> [ "[" <identifier> { "," <identifier> } "]" ]
     */
    private def classTypeIdentifier(): TypeIdentifier = currentToken match {
      case id: ID =>
        eat(IDKIND)
        val tIds = currentToken.kind match {
          case LESSTHAN =>
            eat(LESSTHAN)
            val tmp = commaList(identifier)
            eatRightShiftOrGreaterThan
            tmp.map(x => new TypeIdentifier(x.value, List()))
          case _ => List()
        }
        TypeIdentifier(id.value, tIds).setPos(id)
      case _ => expected(IDKIND)
    }

    /**
     * <typeIdentifier> ::= <identifier> [ "[" <type> { "," <type> } "]" ]
     */
    private def typeIdentifier(): TypeIdentifier = currentToken match {
      case id: ID =>
        eat(IDKIND)
        val tIds = currentToken.kind match {
          case LESSTHAN =>
            eat(LESSTHAN)
            val tmp = commaList(tpe)
            eatRightShiftOrGreaterThan
            tmp
          case _ => List()
        }
        TypeIdentifier(id.value, tIds).setPos(id)
      case _ => expected(IDKIND)
    }


    /**
     * <identifier> ::= sequence of letters, digits and underscores, starting with a letter and which is not a keyword
     */
    private def identifier(): Identifier = currentToken match {
      case id: ID =>
        eat(IDKIND)
        Identifier(id.value).setPos(id)
      case _ => expected(IDKIND)
    }

    /**
     * <stringLit> ::= sequence of arbitrary characters, except new lines and "
     */
    private def stringLit(): StringLit = currentToken match {
      case strlit: STRLIT =>
        eat(STRLITKIND)
        StringLit(strlit.value).setPos(strlit)
      case _ => expected(STRLITKIND)
    }

    /**
     * <intLit> ::= sequence of digits, with no leading zeros
     */
    private def intLit(): IntLit = currentToken match {
      case intlit: INTLIT =>
        eat(INTLITKIND)
        IntLit(intlit.value).setPos(intlit)
      case _ => expected(INTLITKIND)
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
    private def untilNot[T](parse: () => T, kind: TokenKind): List[T] = {
      val condition = () => currentToken.kind == kind
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

}
