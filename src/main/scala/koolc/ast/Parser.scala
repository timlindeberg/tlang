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
    import ctx.reporter._
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
    private def eat(kind: TokenKind*): Unit = {
      for (k <- kind) {
        if (currentToken.kind == k) {
          readToken
        } else {
          expected(k)
        }
      }

    }

    /** Complains that what was found was not expected. The method accepts arbitrarily many arguments of type TokenKind */
    private def expected(kind: TokenKind, more: TokenKind*): Nothing = {
      fatal("expected: " + (kind :: more.toList).mkString(" or ") + ", found: " + currentToken, currentToken)
    }

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

    /**
     * <methodDeclaration> ::= def <identifier> "(" [ <formal> { "," <formal> } ] "): " <tpe> "= {" { <varDeclaration> } { <statement> } return <expression> "}"
     *                       | def <identifier> "(" [ <formal> { "," <formal> } ] ") = {" { <varDeclaration> } { <statement> } "}"
     */
    private def methodDeclaration(): FuncTree = {
      val pos = currentToken
      eat(DEF)
      val id = identifier
      eat(LPAREN)
      val args = commaList(formal)

      eat(RPAREN)
      if(currentToken.kind == COLON){
        eat(COLON)
        val retType = tpe
        eat(EQSIGN, LBRACE)
        val vars = untilNot(varDeclaration, VAR)
        val stmts = until(statement, RETURN)
        eat(RETURN)
        val retExpr = expression
        eat(SEMICOLON, RBRACE)
        MethodDecl(retType, id, args, vars, stmts, retExpr).setPos(pos)
      }else{
        eat(EQSIGN, LBRACE)
        val vars = untilNot(varDeclaration, VAR)
        val stmts = until(statement, RBRACE)
        eat(RBRACE)
        ConstructorDecl(id, args, vars, stmts).setPos(pos)
      }
    }

    /**
     * <tpe> ::= Int[] | Int | Bool | String | <typeIdentifier>
     */
    private def tpe(): TypeTree = {
      val pos = currentToken
      val tree = currentToken.kind match {
        case INT => {
          eat(INT)
          if (currentToken.kind == LBRACKET) {
            eat(LBRACKET, RBRACKET)
            IntArrayType()
          } else {
            IntType()
          }
        }
        case BOOLEAN =>
          eat(BOOLEAN); BooleanType()
        case STRING =>
          eat(STRING); StringType()
        case _ => typeIdentifier
      }
      tree.setPos(pos)
    }

    /**
     * <statement> ::= "{" { <statement> } "}
     *               | if"(" <expression> ")" <statement> [ else <statement> ]
     *               | while"(" <expression> ")" <statement>
     *               | println"(" <expression> ");"
     *               | <identifier> "=" <expression> ";"
     *               | <identifier>"[" <expression> "]" "=" <expression> ";"
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
          (While(expr, statement))
        case PRINTLN =>
          eat(PRINTLN, LPAREN)
          val expr = expression
          eat(RPAREN, SEMICOLON)
          Println(expr)
        case IDKIND =>
          val id = identifier
          currentToken.kind match {
            case EQSIGN =>
              eat(EQSIGN)
              val expr = expression
              eat(SEMICOLON)
              Assign(id, expr)
            case LBRACKET =>
              eat(LBRACKET)
              val expr1 = expression
              eat(RBRACKET, EQSIGN)
              val expr2 = expression
              eat(SEMICOLON)
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
              eat(SEMICOLON)
              e.asInstanceOf[MethodCall]
            case _ => expected(EQSIGN, LBRACKET)
          }
        case _ =>
          val expr = expression
          eat(SEMICOLON)
          expr match {
            case m : MethodCall => m
            case _              => fatal("Not a valid statement, expected println, if, while, assignment or a method call. ", expr)
          }
      }
      tree.setPos(pos)
    }

    /**
     * <expression> ::= <or>
     */
    private def expression(): ExprTree = {
      val pos = currentToken
      val exprMap: Map[TokenKind, (ExprTree, ExprTree) => ExprTree] = Map(
        OR -> Or,
        AND -> And,
        LESSTHAN -> LessThan,
        EQUALS -> Equals,
        PLUS -> Plus,
        MINUS -> Minus,
        TIMES -> Times,
        DIV -> Div)

      /**
       * Parses expressions of type
       * E ::= next { ( kinds[0] | kinds[1] | ... | kinds[n] ) next }.
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

      /** <or> ::= <and> { || <and> } */
      def or() = left(and, OR)

      /** <and> ::= <lessThanEquals> { && <lessThanEquals> } */
      def and() = left(lessThanEquals, AND)

      /** <lessThanEquals> ::= <plusMinus> { ( < | == ) <plusMinus> } */
      def lessThanEquals() = left(plusMinus, LESSTHAN, EQUALS)

      /** <plusMinus> ::= <timesDiv> { ( + | - ) <timesDiv> } */
      def plusMinus() = left(timesDiv, PLUS, MINUS)

      /** <timesDiv> ::= <term> { ( * | / ) <term> } */
      def timesDiv() = left(term, TIMES, DIV)

      /**
       * <term> ::= <termFirst> [ termRest ]
       */
      def term(): ExprTree = {
        /**
         * <termFirst> ::= "(" <expression> ")"
         *               | "!" <expression>
         *               | <intLit>
         *               | <stringLit>
         *               | <identifier>
         *               | true
         *               | false
         *               | this
         *               | new Int"[" <expression> "]"
         *               | new <typeIdentifier> "(" [ <expression> { "," <expression> } ")"
         */
        def termFirst() = {
          val tree = currentToken.kind match {
            case LPAREN =>
              eat(LPAREN); val expr = expression; eat(RPAREN); expr
            case BANG =>
              eat(BANG); Not(term)
            case INTLITKIND =>
              intLit
            case STRLITKIND =>
              stringLit
            case IDKIND =>
              identifier
            case TRUE =>
              eat(TRUE); True()
            case FALSE =>
              eat(FALSE); False()
            case THIS =>
              eat(THIS); This()
            case NEW => {
              eat(NEW)
              if (currentToken.kind == INT) {
                eat(INT, LBRACKET)
                val expr = expression
                eat(RBRACKET)
                NewIntArray(expr)
              } else {
                val id = typeIdentifier
                eat(LPAREN)
                val args = commaList(expression)
                eat(RPAREN)
                New(id, args)
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
         */
        def termRest(lhs: ExprTree): ExprTree = {
          val pos = currentToken
          var e = lhs
          while (currentToken.kind == DOT || currentToken.kind == LBRACKET) {
            if (currentToken.kind == DOT) {
              eat(DOT)
              if (currentToken.kind == LENGTH) {
                eat(LENGTH)
                e = ArrayLength(e)
              } else {
                val id = identifier
                eat(LPAREN)
                val exprs = commaList(expression)
                eat(RPAREN)
                e = MethodCall(e, id, exprs.toList)
              }
            } else if (currentToken.kind == LBRACKET) {
              eat(LBRACKET)
              val expr = expression
              eat(RBRACKET)
              e = ArrayRead(e, expr)
            }
          }
          e.setPos(pos)
        }
        termRest(termFirst)
      }
      or().setPos(pos)
    }

    /**
     * <classTypeIdentifier> ::= <identifier> [ "[" <identifier> { "," <identifier> } "]" ]
     */
    private def classTypeIdentifier(): TypeIdentifier = {
      try {
        val id = currentToken.asInstanceOf[ID]
        eat(IDKIND)
        val tIds = currentToken.kind match {
          case LBRACKET =>
            eat(LBRACKET)
            val tmp = commaList(identifier)
            eat(RBRACKET)
            tmp.map(x => new TypeIdentifier(x.value, List()))
          case _ => List()
        }
        TypeIdentifier(id.value, tIds).setPos(id)
      } catch {
        case _: ClassCastException => expected(IDKIND)
      }
    }
    
    /**
     * <typeIdentifier> ::= <identifier> [ "[" <type> { "," <type> } "]" ]
     */
    private def typeIdentifier(): TypeIdentifier = {
      try {
        val id = currentToken.asInstanceOf[ID]
        eat(IDKIND)
        val tIds = currentToken.kind match {
          case LBRACKET =>
            eat(LBRACKET)
            val tmp = commaList(tpe)
            eat(RBRACKET)
            tmp
          case _ => List()
        }
        TypeIdentifier(id.value, tIds).setPos(id)
      } catch {
        case _: ClassCastException => expected(IDKIND)
      }
    }

    /**
     * <identifier> ::= sequence of letters, digits and underscores, starting with a letter and which is not a keyword
     */
    private def identifier(): Identifier = {
      try {
        val id = currentToken.asInstanceOf[ID]
        eat(IDKIND)
        Identifier(id.value).setPos(id)
      } catch {
        case _: ClassCastException => expected(IDKIND)
      }
    }

    /**
     * <stringLit> ::= sequence of arbitrary characters, except new lines and "
     */
    private def stringLit(): StringLit = {
      try {
        val id = currentToken.asInstanceOf[STRLIT]
        eat(STRLITKIND)
        StringLit(id.value).setPos(id)
      } catch {
        case _: ClassCastException => expected(STRLITKIND)
      }
    }

    /**
     * <intLit> ::= sequence of digits, with no leading zeros
     */
    private def intLit(): IntLit = {
      try {
        val id = currentToken.asInstanceOf[INTLIT]
        eat(INTLITKIND)
        IntLit(id.value).setPos(id)
      } catch {
        case _: ClassCastException => expected(INTLITKIND)
      }
    }

    /**
     * Parses a commalist of the form
     * <commaList> ::= [ parse { "," parse } ]
     */
    private def commaList[T](parse: () => T): List[T] = {
      if (currentToken.kind == RPAREN) {
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
    private def until[T](parse: () => T, kind: TokenKind): List[T] = {
      val condition = () => currentToken.kind != kind
      _until(condition, parse, kind)
    }

    /**
     * Continues parsing until a token different from the given token is encountered.
     */
    private def untilNot[T](parse: () => T, kind: TokenKind): List[T] = {
      val condition = () => currentToken.kind == kind
      _until(condition, parse, kind)
    }

    private def _until[T](condition: () => Boolean, parse: () => T, kind: TokenKind) = {
      var arrBuff = new ArrayBuffer[T]()
      while (condition()) {
        arrBuff += parse()
      }
      arrBuff.toList
    }
  }

}
