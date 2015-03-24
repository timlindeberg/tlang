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

    def parseGoal() = {
      val pos = currentToken
      Program(parseMainObject, until(parseClassDecleration, EOF)).setPos(pos)
    }

    def parseMainObject(): MainObject = {
      val pos = currentToken
      eat(OBJECT)
      val id = parseIdentifier
      eat(LBRACE, DEF, MAIN, LPAREN, RPAREN, COLON, UNIT, EQSIGN, LBRACE)
      val stmts = until(parseStatement, RBRACE)
      eat(RBRACE, RBRACE)
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
      eat(RBRACE)
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
      eat(RETURN)
      val retExpr = parseExpression
      eat(SEMICOLON, RBRACE)
      MethodDecl(retType, id, args, vars, stmts, retExpr).setPos(pos)
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
      //println(currentToken.kind)
      val pos = currentToken
      currentToken.kind match {
        case LBRACE => {
          eat(LBRACE)
          val stmts = until(parseStatement, RBRACE)
          eat(RBRACE)
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
            case _ => expected(EQSIGN, LBRACKET)
          }
        }
        case _ => expected(LBRACE, IF, WHILE, PRINTLN, IDKIND)
      }
    }

    def parseExpression(): ExprTree = {
      val pos = currentToken
      parseBooleanOperator.setPos(pos)
    }

    def leftAssoc(pos: Token, lhs: ExprTree, parse: () => ExprTree, kind: TokenKind, f: (ExprTree, ExprTree) => ExprTree): ExprTree = {
      var e = lhs
      while (currentToken.kind == kind) {
        val pos2 = currentToken
        eat(kind)
        e = f(e, parse()).setPos(pos2)
      }
      e.setPos(pos)
    }

    def parseBooleanOperator(): ExprTree = {
      val pos = currentToken
      var e = parseBooleanOperator2

      while (currentToken.kind == OR) {
        val pos2 = currentToken
        eat(OR)
        e = Or(e, parseBooleanOperator2).setPos(pos2)
      }
      e
    }

    def parseBooleanOperator2(): ExprTree = {
      val pos = currentToken
      var e = parseLessThanEquals

      while (currentToken.kind == AND) {
        val pos2 = currentToken
        eat(AND)
        e = And(e, parseLessThanEquals).setPos(pos2)
      }
      e
    }

    def parseLessThanEquals(): ExprTree = {
      val pos = currentToken
      var e = parsePlusMinus

      while (currentToken.kind == LESSTHAN || currentToken.kind == EQUALS) {
        val pos2 = currentToken
        if (currentToken.kind == LESSTHAN) {
          eat(LESSTHAN)
          e = LessThan(e, parsePlusMinus).setPos(pos2)
        } else if (currentToken.kind == EQUALS) {
          eat(EQUALS)
          e = Equals(e, parsePlusMinus).setPos(pos2)
        }
      }
      e
    }

    def parsePlusMinus(): ExprTree = {
      val pos = currentToken
      var e = parseTimesDiv

      while (currentToken.kind == PLUS || currentToken.kind == MINUS) {
        val pos2 = currentToken
        if (currentToken.kind == PLUS) {
          eat(PLUS)
          e = Plus(e, parseTimesDiv).setPos(pos2)
        } else if (currentToken.kind == MINUS) {
          eat(MINUS)
          e = Minus(e, parseTimesDiv).setPos(pos2)
        }
      }
      e
    }

    def parseTimesDiv(): ExprTree = {
      val pos = currentToken
      var e = parseTerm

      while (currentToken.kind == TIMES || currentToken.kind == DIV) {
        val pos2 = currentToken
        if (currentToken.kind == TIMES) {
          eat(TIMES)
          e = Times(e, parseTerm).setPos(pos2)
        } else if (currentToken.kind == DIV) {
          eat(DIV)
          e = Div(e, parseTerm).setPos(pos2)
        }
      }
      e
    }

    def parseTerm(): ExprTree = {
      val pos = currentToken
      parseTermRest(currentToken.kind match {
        case LPAREN =>
          eat(LPAREN); var expr = parseExpression; eat(RPAREN); expr.setPos(pos)
        case BANG =>
          eat(BANG); Not(parseTerm).setPos(pos)
        case INTLITKIND =>
          parseIntLit.setPos(pos)
        case STRLITKIND =>
          parseStringLit.setPos(pos)
        case IDKIND =>
          parseIdentifier.setPos(pos)
        case TRUE =>
          eat(TRUE); True().setPos(pos)
        case FALSE =>
          eat(FALSE); False().setPos(pos)
        case THIS =>
          eat(THIS); This().setPos(pos)
        case NEW => {
          eat(NEW)
          if (currentToken.kind == INT) {
            eat(INT, LBRACKET)
            var expr = parseExpression
            eat(RBRACKET)
            NewIntArray(expr).setPos(pos)
          } else {
            var id = parseIdentifier
            eat(LPAREN, RPAREN)
            New(id).setPos(pos)
          }
        }
        case _ => expected(BANG, BANG)
      })
    }

    def parseTermRest(lhs: ExprTree): ExprTree = {
      val pos = currentToken
      var e = lhs
      while (currentToken.kind == DOT || currentToken.kind == LBRACKET) {
        if (currentToken.kind == DOT) {
          eat(DOT)
          if (currentToken.kind == LENGTH) {
            eat(LENGTH)
            e = ArrayLength(e).setPos(pos)
          } else {
            val id = parseIdentifier
            eat(LPAREN)
            val exprs = commaList(parseExpression)
            eat(RPAREN)
            e = MethodCall(e, id, exprs.toList).setPos(pos)
          }
        } else if (currentToken.kind == LBRACKET) {
          eat(LBRACKET)
          var expr = parseExpression
          eat(RBRACKET)
          e = ArrayRead(e, expr).setPos(pos)
        }
      }
      e
    }

    def parseExpressionRest(lhs: ExprTree): ExprTree = {

      val pos = currentToken

      def leftAssoc(kind: TokenKind, f: (ExprTree, ExprTree) => ExprTree): ExprTree = {
        var e = lhs
        while (currentToken.kind == kind) {
          val pos2 = currentToken
          eat(kind)
          e = f(e, parseTerm)
        }
        e.setPos(pos)
      }

      currentToken.kind match {
        case TIMES =>
          leftAssoc(TIMES, Times(_, _))
        case DIV =>
          leftAssoc(DIV, Div(_, _))
        case PLUS =>
          leftAssoc(PLUS, Plus(_, _))
        case MINUS =>
          leftAssoc(MINUS, Minus(_, _))
        case LESSTHAN =>
          leftAssoc(LESSTHAN, LessThan(_, _))
        case EQUALS =>
          leftAssoc(EQUALS, Equals(_, _))
        case AND =>
          leftAssoc(AND, And(_, _))
        case OR =>
          leftAssoc(OR, Or(_, _))
        case DOT => {
          eat(DOT)
          if (currentToken.kind == LENGTH) {
            eat(LENGTH)
            parseExpressionRest(ArrayLength(lhs).setPos(pos))
          } else {
            val id = parseIdentifier
            eat(LPAREN)
            val exprs = commaList(parseExpression)
            eat(RPAREN)
            parseExpressionRest(MethodCall(lhs, id, exprs.toList).setPos(pos))
          }
        }
        case LBRACKET => {
          eat(LBRACKET)
          var expr = parseExpression
          eat(RBRACKET)
          parseExpressionRest(ArrayRead(lhs, expr).setPos(pos))
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

    def optional[T](parse: () => T, kind: TokenKind): Option[T] = {
      if (currentToken.kind == kind) {
        eat(kind)
        Some(parse())
      } else {
        None
      }
    }

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
      while (condition()) arrBuff += parse()
      arrBuff.toList
    }
  }

}
