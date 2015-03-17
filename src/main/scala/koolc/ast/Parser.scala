package koolc
package ast

import utils._
import Trees._
import lexer._
import lexer.Tokens._
import scala.collection.mutable.ArrayBuffer
import org.xml.sax.helpers.NewInstance

object Parser extends Pipeline[Iterator[Token], Program] {
  
 var currentToken: Token = new Token(BAD)
     
  def run(ctx: Context)(tokens: Iterator[Token]): Program = {
    import ctx.reporter._
    val astBuilder = new ASTBuilder(ctx, tokens)
    astBuilder.readToken
    val tree = astBuilder.parseGoal
    terminateIfErrors
    tree
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

    def parseGoal() = Program(parseMainObject, until(parseClassDecleration, EOF))

    def parseMainObject(): MainObject = {
      eat(OBJECT)
      val id = parseIdentifier
      eat(LBRACE, DEF, MAIN, LPAREN, RPAREN, COLON, UNIT, EQSIGN, LBRACE)
      val stmts = until(parseStatement, RBRACE)
      eat(RBRACE)
      MainObject(id, stmts)
    }

    def parseClassDecleration(): ClassDecl = {
      eat(CLASS)
      var id = parseIdentifier
      var parent = optional(parseIdentifier, EXTENDS)
      eat(LBRACE)
      var vars = until(parseVarDeclaration, VAR)
      var methods = until(parseMethodDeclaration, VAR)
      ClassDecl(id, parent, vars, methods)
    }

    def parseVarDeclaration(): VarDecl = {
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

    def parseMethodDeclaration(): MethodDecl = {
      eat(DEF)
      val id = parseIdentifier
      eat(LPAREN)
      var args = commaList(parseFormal)

      eat(RPAREN, COLON)
      val retType = parseType
      eat(EQSIGN, LBRACE)
      val vars = untilNot(parseVarDeclaration, VAR)
      val stmts = until(parseStatement, RETURN)
      val expr = parseExpression
      MethodDecl(retType, id, args, vars, stmts, expr)
    }

    def parseType(): TypeTree = currentToken.kind match {
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

    def parseStatement(): StatTree = currentToken.kind match {
      case LBRACE => {
        eat(LBRACE)
        val stmts = until(parseStatement, RBRACE) 
        Block(stmts)
      }
      case IF => {
        eat(IF, LPAREN)
        val expr = parseExpression
        eat(RPAREN)
        val stmt = parseStatement
        val els = optional(parseStatement, ELSE)
        If(expr, stmt, els)
      }
      case WHILE => {
        eat(WHILE, LPAREN)
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

    def parseExpression(): ExprTree = {
      currentToken.kind match {
        case INTLITKIND => eat(INTLITKIND); parseExpressionRest(parseIntLit)
        case STRLITKIND => eat(STRLITKIND); parseExpressionRest(parseStringLit)
        case IDKIND     => eat(IDKIND);     parseExpressionRest(parseIdentifier)
        case TRUE       => eat(TRUE);       parseExpressionRest(True())
        case FALSE      => eat(FALSE);      parseExpressionRest(False())
        case THIS       => eat(THIS);       parseExpressionRest(This())
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
        case AND      => eat(AND);      And(lhs, parseExpression)
        case OR       => eat(OR);       Or(lhs, parseExpression)
        case EQUALS   => eat(EQUALS);   Equals(lhs, parseExpression)
        case LESSTHAN => eat(LESSTHAN); LessThan(lhs, parseExpression)
        case PLUS     => eat(PLUS);     Plus(lhs, parseExpression)
        case MINUS    => eat(MINUS);    Minus(lhs, parseExpression)
        case TIMES    => eat(TIMES);    Times(lhs, parseExpression)
        case DIV      => eat(DIV);      Div(lhs, parseExpression)
        case DOT => {
          eat(DOT)
          if (currentToken.kind == LENGTH) {
            eat(LENGTH)
            ArrayLength(lhs)
          } else {
            val id = parseIdentifier
            eat(LPAREN)
            val exprs = commaList(parseExpression)
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

    def parseIdentifier(): Identifier = {
      try {
        val id = currentToken.asInstanceOf[ID]
        eat(IDKIND)
        Identifier(id.value)
      } catch {
        case _ => expected(IDKIND)
      }
    }

    def parseStringLit(): StringLit = {
      try {
        val id = currentToken.asInstanceOf[STRLIT]
        eat(STRLITKIND)
        StringLit(id.value)
      } catch {
        case _ => expected(STRLITKIND)
      }
    }

    def parseIntLit(): IntLit = {
      try {
        val id = currentToken.asInstanceOf[INTLIT]
        eat(INTLITKIND)
        IntLit(id.value)
      } catch {
        case _ => expected(INTLITKIND)
      }
    }
    
    def commaList[T](parse: () => T): List[T] = {
      val arrBuff = new ArrayBuffer[T]
      arrBuff += parse()
      untilNot(parse, COMMA, arrBuff)
    }
    
    def optional[T](parse: () => T, kind: TokenKind): Option[T] = if (currentToken.kind == kind) Some(parse()) else None

    def until[T](parse: () => T, kind: TokenKind, arrBuff: ArrayBuffer[T] = new ArrayBuffer[T]()): List[T] =
      _until(() => currentToken.kind != kind, parse, kind, arrBuff)

    def untilNot[T](parse: () => T, kind: TokenKind, arrBuff: ArrayBuffer[T] = new ArrayBuffer[T]()): List[T] =
      _until(() => currentToken.kind == kind, parse, kind, arrBuff)

    private def _until[T](condition: () => Boolean, parse: () => T, kind: TokenKind,
        arrBuff: ArrayBuffer[T] = new ArrayBuffer[T]()) = {
      while (condition()) arrBuff += parse()
      eat(kind)
      arrBuff.toList
    }
 }
  
}
