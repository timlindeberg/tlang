package koolc
package ast

import utils._
import Trees._
import lexer._
import lexer.Tokens._
import scala.collection.mutable.ArrayBuffer
import org.xml.sax.helpers.NewInstance

object Parser extends Pipeline[Iterator[Token], Program] {
  def run(ctx: Context)(tokens: Iterator[Token]): Program = {
    import ctx.reporter._

    /** Store the current token, as read from the lexer. */
    var currentToken: Token = new Token(BAD)

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
    def eat(kind: TokenKind*): Unit = {
      for (k <- kind) {
        if (currentToken.kind == k) {
          readToken
        } else {
          expected(k)
        }
      }

    }

    /** Complains that what was found was not expected. The method accepts arbitrarily many arguments of type TokenKind */
    def expected(kind: TokenKind, more: TokenKind*): Nothing = {
      fatal("expected: " + (kind :: more.toList).mkString(" or ") + ", found: " + currentToken, currentToken)
    }

    def parseGoal = {
      val mainObject = parseMainObject
      var classDecls = new ArrayBuffer[ClassDecl]
      while (currentToken.kind != EOF) {
        classDecls += parseClassDecleration
      }
      Program(mainObject, classDecls.toList)
    }

    def parseMainObject: MainObject = {
      eat(OBJECT)
      val id = parseIdentifier
      eat(LBRACE, DEF, MAIN, LPAREN, RPAREN, COLON, UNIT, EQSIGN, LBRACE)
      val stmts = parseStatementUntil(RBRACE)
      eat(RBRACE)
      MainObject(id, stmts)
    }

    def parseClassDecleration: ClassDecl = {
      eat(CLASS)
      var id = parseIdentifier
      var parent = if (currentToken.kind == EXTENDS) Some(parseIdentifier) else None
      eat(LBRACE)
      var vars = new ArrayBuffer[VarDecl]
      while (currentToken.kind == VAR) {
        vars += parseVarDeclaration
      }
      var methods = new ArrayBuffer[MethodDecl]
      while (currentToken.kind != RBRACE) {
        methods += parseMethodDeclaration
      }
      ClassDecl(id, parent, vars.toList, methods.toList)
    }

    def parseVarDeclaration: VarDecl = {
      eat(VAR)
      val id = parseIdentifier
      eat(COLON)
      val typ = parseType
      eat(SEMICOLON)
      VarDecl(typ, id)
    }

    def parseFormal(): Formal = {
      var id = parseIdentifier
      eat(COLON)
      var typ = parseType
      Formal(typ, id)
    }

    def parseMethodDeclaration: MethodDecl = {
      eat(DEF)
      val id = parseIdentifier
      eat(LPAREN)
      var args = new ArrayBuffer[Formal]()
      args += parseFormal
      while (currentToken.kind != COMMA) {
        eat(COMMA)
        args += parseFormal
      }
      eat(RPAREN, COLON)
      val retType = parseType
      eat(EQSIGN, LBRACE)
      val vars = new ArrayBuffer[VarDecl]()
      while (currentToken.kind == VAR) {
        vars += parseVarDeclaration
      }
      val stmts = new ArrayBuffer[StatTree]()
      while (currentToken.kind != RETURN) {
        stmts += parseStatement
      }
      eat(RETURN)
      val expr = parseExpression
      MethodDecl(retType, id, args.toList, vars.toList, stmts.toList, expr)
    }

    def parseType: TypeTree = currentToken.kind match {
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
      case _ => parseIdentifier
    }

    def parseStatementUntil(kind: TokenKind): List[StatTree] = {
      var stmts = new ArrayBuffer[StatTree]()
      while (currentToken.kind != kind) {
        stmts += parseStatement
      }
      eat(kind)
      stmts.toList
    }

    def parseStatement: StatTree = currentToken.kind match {
      case LBRACE => {
        eat(LBRACE)
        Block(parseStatementUntil(RBRACE))
      }
      case IF => {
        eat(IF, LPAREN)
        val expr = parseExpression
        eat(RPAREN)
        val stmt = parseStatement
        If(expr, stmt, if (currentToken.kind == ELSE) Some(parseStatement) else None)
      }
      case WHILE => {
        eat(WHILE)
        eat(LPAREN)
        val expr = parseExpression
        eat(RPAREN)
        While(expr, parseStatement)
      }
      case PRINTLN => {
        eat(PRINTLN, LPAREN)
        val expr = parseExpression
        eat(RPAREN, SEMICOLON)
        Println(expr)
      }
      case IDKIND => {
        val id = parseIdentifier
        currentToken.kind match {
          case EQSIGN => {
            eat(EQSIGN)
            val expr = parseExpression
            eat(SEMICOLON)
            Assign(id, expr)
          }
          case LBRACKET => {
            eat(LBRACKET)
            val expr1 = parseExpression
            eat(RBRACKET, EQSIGN)
            val expr2 = parseExpression
            eat(SEMICOLON)
            ArrayAssign(id, expr1, expr2)
          }
        }
      }
      case _ => expected(LBRACE, IF, WHILE, PRINTLN, IDKIND)
    }

    def parseExpression: ExprTree = {
      currentToken.kind match {
        case INTLITKIND => parseExpressionRest(parseIntLit)
        case STRLITKIND => parseExpressionRest(parseStringLit)
        case IDKIND     => parseExpressionRest(parseIdentifier)
        case TRUE       => parseExpressionRest(True())
        case FALSE      => parseExpressionRest(False())
        case THIS       => parseExpressionRest(This())
        case NEW => {
          eat(NEW)
          if (currentToken.kind == INT) {
            eat(INT, LBRACKET)
            var expr = parseExpression
            eat(RBRACKET)
            parseExpressionRest(NewIntArray(expr))
          } else {
            eat(IDKIND)
            var id = parseIdentifier
            eat(LBRACKET, RBRACKET)
            parseExpressionRest(New(id))
          }
        }
        case BANG => Not(parseExpression)
        case LPAREN =>
          eat(LPAREN); var expr = parseExpression; eat(RPAREN); expr
      }
    }

    def parseExpressionRest(lhs: ExprTree): ExprTree = {
      currentToken.kind match {
        case AND      => And(lhs, parseExpression)
        case OR       => Or(lhs, parseExpression)
        case EQUALS   => Equals(lhs, parseExpression)
        case LESSTHAN => LessThan(lhs, parseExpression)
        case PLUS     => Plus(lhs, parseExpression)
        case MINUS    => Minus(lhs, parseExpression)
        case TIMES    => Times(lhs, parseExpression)
        case DIV      => Div(lhs, parseExpression)
        case DOT => {
          eat(DOT)
          if (currentToken.kind == LENGTH) {
            eat(LENGTH)
            ArrayLength(lhs)
          } else {
            val id = parseIdentifier
            eat(LPAREN)
            val exprs = new ArrayBuffer[ExprTree]
            exprs += parseExpression
            while (currentToken.kind == COMMA) {
              exprs += parseExpression
            }
            eat(RPAREN)
            MethodCall(lhs, id, exprs.toList)
          }
        }
        case LBRACKET => {
          eat(LBRACKET)
          var expr = parseExpression
          eat(RBRACKET)
          ArrayRead(lhs, expr)
        }
        case _ => lhs
      }
    }

    def parseIdentifier: Identifier = {
      try {
        val id = currentToken.asInstanceOf[ID]
        eat(IDKIND)
        Identifier(id.value)
      } catch {
        case _ => expected(IDKIND)
      }
    }

    def parseStringLit: StringLit = {
      try {
        val id = currentToken.asInstanceOf[STRLIT]
        eat(STRLITKIND)
        StringLit(id.value)
      } catch {
        case _ => expected(STRLITKIND)
      }
    }

    def parseIntLit: IntLit = {
      try {
        val id = currentToken.asInstanceOf[INTLIT]
        eat(INTLITKIND)
        IntLit(id.value)
      } catch {
        case _ => expected(INTLITKIND)
      }
    }

    readToken
    val tree = parseGoal
    terminateIfErrors
    tree
  }
}
