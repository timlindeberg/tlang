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
      val pos = currentToken
      eat(OBJECT)
      val id = parseIdentifier
      eat(LBRACE, DEF, MAIN, LPAREN, RPAREN, COLON, UNIT, EQSIGN, LBRACE)
      val stmts = until(parseStatement, RBRACE)
      eat(RBRACE)
      eat(RBRACE)
      MainObject(id, stmts).setPos(pos)
    }

    def parseClassDecleration(): ClassDecl = {
      val pos = currentToken
      eat(CLASS)
      var id = parseIdentifier
      var parent = optional(parseIdentifier, EXTENDS)
      eat(LBRACE)
      var vars = untilNot(parseVarDeclaration, VAR)
      var methods = untilNot(parseMethodDeclaration, DEF)
      ClassDecl(id, parent, vars, methods).setPos(pos)
    }

    def parseVarDeclaration(): VarDecl = {
      val pos = currentToken
      eat(VAR)
      val id = parseIdentifier
      eat(COLON)
      val typ = parseType
      eat(SEMICOLON)
      VarDecl(typ, id).setPos(pos)
    }

    def parseFormal(): Formal = {
      val pos = currentToken
      var id = parseIdentifier
      eat(COLON)
      var typ = parseType
      Formal(typ, id).setPos(pos)
    }

    def parseMethodDeclaration(): MethodDecl = {
      val pos = currentToken
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
      MethodDecl(retType, id, args, vars, stmts, expr).setPos(pos)
    }

    def parseType(): TypeTree = {
      val pos = currentToken
      currentToken.kind match {

        case INT => {
          eat(INT)
          if (currentToken.kind == LBRACKET) {
            eat(LBRACKET, RBRACKET)
            IntArrayType().setPos(pos)
          } else {
            IntType().setPos(pos)
          }
        }
        case BOOLEAN =>
          eat(BOOLEAN); BooleanType().setPos(pos)
        case STRING =>
          eat(STRING); StringType().setPos(pos)
        case _ => parseIdentifier
      }
    }

    def parseStatement(): StatTree = {
      val pos = currentToken
      currentToken.kind match {
        case LBRACE => {
          eat(LBRACE)
          val stmts = until(parseStatement, RBRACE)
          Block(stmts).setPos(pos)
        }
        case IF => {
          eat(IF, LPAREN)
          val expr = parseExpression
          eat(RPAREN)
          val stmt = parseStatement
          val els = optional(parseStatement, ELSE)
          If(expr, stmt, els).setPos(pos)
        }
        case WHILE => {
          eat(WHILE, LPAREN)
          val expr = parseExpression
          eat(RPAREN)
          While(expr, parseStatement).setPos(pos)
        }
        case PRINTLN => {
          eat(PRINTLN, LPAREN)
          val expr = parseExpression
          eat(RPAREN, SEMICOLON)
          Println(expr).setPos(pos)
        }
        case IDKIND => {
          val id = parseIdentifier
          currentToken.kind match {
            case EQSIGN => {
              eat(EQSIGN)
              val expr = parseExpression
              eat(SEMICOLON)
              Assign(id, expr).setPos(pos)
            }
            case LBRACKET => {
              eat(LBRACKET)
              val expr1 = parseExpression
              eat(RBRACKET, EQSIGN)
              val expr2 = parseExpression
              eat(SEMICOLON)
              ArrayAssign(id, expr1, expr2).setPos(pos)
            }
          }
        }
        case _ => expected(LBRACE, IF, WHILE, PRINTLN, IDKIND)
      }
    }

    def parseExpression(): ExprTree = {
      val pos = currentToken
      currentToken.kind match {
        case INTLITKIND =>
          eat(INTLITKIND); parseExpressionRest(parseIntLit.setPos(pos))
        case STRLITKIND =>
          eat(STRLITKIND); parseExpressionRest(parseStringLit.setPos(pos))
        case IDKIND =>
          eat(IDKIND); parseExpressionRest(parseIdentifier.setPos(pos))
        case TRUE =>
          eat(TRUE); parseExpressionRest(True().setPos(pos))
        case FALSE =>
          eat(FALSE); parseExpressionRest(False().setPos(pos))
        case THIS =>
          eat(THIS); parseExpressionRest(This().setPos(pos))
        case NEW => {
          eat(NEW)
          if (currentToken.kind == INT) {
            eat(INT, LBRACKET)
            var expr = parseExpression
            eat(RBRACKET)
            parseExpressionRest(NewIntArray(expr).setPos(pos))
          } else {
            eat(IDKIND)
            var id = parseIdentifier
            eat(LBRACKET, RBRACKET)
            parseExpressionRest(New(id).setPos(pos))
          }
        }
        case BANG   => Not(parseExpression).setPos(pos)
        case LPAREN => eat(LPAREN); var expr = parseExpression; eat(RPAREN); expr
      }
    }

    def parseExpressionRest(lhs: ExprTree): ExprTree = {
      val pos = currentToken
      currentToken.kind match {
        case AND =>
          eat(AND); And(lhs, parseExpression).setPos(pos)
        case OR =>
          eat(OR); Or(lhs, parseExpression).setPos(pos)
        case EQUALS =>
          eat(EQUALS); Equals(lhs, parseExpression).setPos(pos)
        case LESSTHAN =>
          eat(LESSTHAN); LessThan(lhs, parseExpression).setPos(pos)
        case PLUS =>
          eat(PLUS); Plus(lhs, parseExpression).setPos(pos)
        case MINUS =>
          eat(MINUS); Minus(lhs, parseExpression).setPos(pos)
        case TIMES =>
          eat(TIMES); Times(lhs, parseExpression).setPos(pos)
        case DIV =>
          eat(DIV); Div(lhs, parseExpression).setPos(pos)
        case DOT => {
          eat(DOT)
          if (currentToken.kind == LENGTH) {
            eat(LENGTH)
            ArrayLength(lhs).setPos(pos)
          } else {
            val id = parseIdentifier
            eat(LPAREN)
            val exprs = commaList(parseExpression)
            eat(RPAREN)
            MethodCall(lhs, id, exprs.toList).setPos(pos)
          }
        }
        case LBRACKET => {
          eat(LBRACKET)
          var expr = parseExpression
          eat(RBRACKET)
          ArrayRead(lhs, expr).setPos(pos)
        }
        case _ => lhs
      }
    }

    def parseIdentifier(): Identifier = {
      try {
        val id = currentToken.asInstanceOf[ID]
        val t = currentToken
        eat(IDKIND)
        Identifier(id.value).setPos(t)
      } catch {
        case _: Throwable => expected(IDKIND)
      }
    }

    def parseStringLit(): StringLit = {
      try {
        val id = currentToken.asInstanceOf[STRLIT]
        val t = currentToken
        eat(STRLITKIND)
        StringLit(id.value).setPos(t)
      } catch {
        case _: Throwable => expected(STRLITKIND)
      }
    }

    def parseIntLit(): IntLit = {
      try {
        val id = currentToken.asInstanceOf[INTLIT]
        val t = currentToken
        eat(INTLITKIND)
        IntLit(id.value).setPos(t)
      } catch {
        case _: Throwable => expected(INTLITKIND)
      }
    }

    def commaList[T](parse: () => T): List[T] = {
      val arrBuff = new ArrayBuffer[T]
      arrBuff += parse()
      untilNot(parse, COMMA, arrBuff)
    }

    def optional[T](parse: () => T, kind: TokenKind): Option[T] = if (currentToken.kind == kind) Some(parse()) else None

    def until[T](parse: () => T, kind: TokenKind, arrBuff: ArrayBuffer[T] = new ArrayBuffer[T]()): List[T] = {
      val condition = () => currentToken.kind != kind
      _until(condition, parse, kind, arrBuff)
    }

    def untilNot[T](parse: () => T, kind: TokenKind, arrBuff: ArrayBuffer[T] = new ArrayBuffer[T]()): List[T] = {
      val condition = () => currentToken.kind == kind
      _until(condition, parse, kind, arrBuff)
    }

    private def _until[T](condition: () => Boolean, parse: () => T, kind: TokenKind,
                          arrBuff: ArrayBuffer[T] = new ArrayBuffer[T]()) = {
      println(currentToken.kind + " " + kind)
      println("cond: " + condition())
      while (condition()) arrBuff += parse()
      if (!arrBuff.isEmpty) {
        eat(kind)
      }
      println(arrBuff.size)
      arrBuff.toList
    }
  }

}
