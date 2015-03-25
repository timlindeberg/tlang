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
      Program(mainObject, until(classDecleration, EOF)).setPos(pos)
    }

    def mainObject(): MainObject = {
      val pos = currentToken
      eat(OBJECT)
      val id = identifier
      eat(LBRACE, DEF, MAIN, LPAREN, RPAREN, COLON, UNIT, EQSIGN, LBRACE)
      val stmts = until(statement, RBRACE)
      eat(RBRACE, RBRACE)
      MainObject(id, stmts).setPos(pos)
    }

    def classDecleration(): ClassDecl = {
      val pos = currentToken
      eat(CLASS)
      var id = identifier
      var parent = optional(identifier, EXTENDS)
      eat(LBRACE)
      var vars = untilNot(varDeclaration, VAR)
      var methods = untilNot(methodDeclaration, DEF)
      eat(RBRACE)
      ClassDecl(id, parent, vars, methods).setPos(pos)
    }

    def varDeclaration(): VarDecl = {
      val pos = currentToken
      eat(VAR)
      val id = identifier
      eat(COLON)
      val typ = tpe
      eat(SEMICOLON)
      VarDecl(typ, id).setPos(pos)
    }

    def formal(): Formal = {
      val pos = currentToken
      var id = identifier
      eat(COLON)
      var typ = tpe
      Formal(typ, id).setPos(pos)
    }

    def methodDeclaration(): MethodDecl = {
      val pos = currentToken
      eat(DEF)
      val id = identifier
      eat(LPAREN)
      var args = commaList(formal)

      eat(RPAREN, COLON)
      val retType = tpe
      eat(EQSIGN, LBRACE)
      val vars = untilNot(varDeclaration, VAR)
      val stmts = until(statement, RETURN)
      eat(RETURN)
      val retExpr = expression
      eat(SEMICOLON, RBRACE)
      MethodDecl(retType, id, args, vars, stmts, retExpr).setPos(pos)
    }

    def tpe(): TypeTree = {
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
        case _ => identifier
      }
    }

    def statement(): StatTree = {
      //println(currentToken.kind)
      val pos = currentToken
      currentToken.kind match {
        case LBRACE => {
          eat(LBRACE)
          val stmts = until(statement, RBRACE)
          eat(RBRACE)
          Block(stmts).setPos(pos)
        }
        case IF => {
          eat(IF, LPAREN)
          val expr = expression
          eat(RPAREN)
          val stmt = statement
          val els = optional(statement, ELSE)
          If(expr, stmt, els).setPos(pos)
        }
        case WHILE => {
          eat(WHILE, LPAREN)
          val expr = expression
          eat(RPAREN)
          (While(expr, statement)).setPos(pos)
        }
        case PRINTLN => {
          eat(PRINTLN, LPAREN)
          val expr = expression
          eat(RPAREN, SEMICOLON)
          Println(expr).setPos(pos)
        }
        case IDKIND => {
          val id = identifier
          currentToken.kind match {
            case EQSIGN => {
              eat(EQSIGN)
              val expr = expression
              eat(SEMICOLON)
              Assign(id, expr).setPos(pos)
            }
            case LBRACKET => {
              eat(LBRACKET)
              val expr1 = expression
              eat(RBRACKET, EQSIGN)
              val expr2 = expression
              eat(SEMICOLON)
              ArrayAssign(id, expr1, expr2).setPos(pos)
            }
            case _ => expected(EQSIGN, LBRACKET)
          }
        }
        case _ => expected(LBRACE, IF, WHILE, PRINTLN, IDKIND)
      }
    }

    def expression(): ExprTree = {
      val pos = currentToken

      def left(f: (ExprTree, ExprTree) => ExprTree, next: () => ExprTree, kind: TokenKind): ExprTree = {
        var e = next()
        while (currentToken.kind == kind) {
          val pos = currentToken
          eat(kind)
          e = f(e, next()).setPos(pos)
        }
        e
      }

      def or() = left(Or, and, OR)
      def and() = left(And, lessThanAndEquals, AND)

      def lessThanAndEquals(): ExprTree = {
        val pos = currentToken
        var e = plusAndMinus

        while (currentToken.kind == LESSTHAN || currentToken.kind == EQUALS) {
          val pos2 = currentToken
          if (currentToken.kind == LESSTHAN) {
            eat(LESSTHAN)
            e = LessThan(e, plusAndMinus).setPos(pos2)
          } else if (currentToken.kind == EQUALS) {
            eat(EQUALS)
            e = Equals(e, plusAndMinus).setPos(pos2)
          }
        }
        e
      }

      def plusAndMinus(): ExprTree = {
        val pos = currentToken
        var e = timesAndDiv

        while (currentToken.kind == PLUS || currentToken.kind == MINUS) {
          val pos2 = currentToken
          if (currentToken.kind == PLUS) {
            eat(PLUS)
            e = Plus(e, timesAndDiv).setPos(pos2)
          } else if (currentToken.kind == MINUS) {
            eat(MINUS)
            e = Minus(e, timesAndDiv).setPos(pos2)
          }
        }
        e
      }

      def timesAndDiv(): ExprTree = {
        val pos = currentToken
        var e = term

        while (currentToken.kind == TIMES || currentToken.kind == DIV) {
          val pos2 = currentToken
          if (currentToken.kind == TIMES) {
            eat(TIMES)
            e = Times(e, term).setPos(pos2)
          } else if (currentToken.kind == DIV) {
            eat(DIV)
            e = Div(e, term).setPos(pos2)
          }
        }
        e
      }

      def term(): ExprTree = {
        val pos = currentToken
        termRest(currentToken.kind match {
          case LPAREN =>
            eat(LPAREN); var expr = expression; eat(RPAREN); expr.setPos(pos)
          case BANG =>
            eat(BANG); (Not(term)).setPos(pos)
          case INTLITKIND =>
            intLit.setPos(pos)
          case STRLITKIND =>
            stringLit.setPos(pos)
          case IDKIND =>
            identifier.setPos(pos)
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
              var expr = expression
              eat(RBRACKET)
              NewIntArray(expr).setPos(pos)
            } else {
              var id = identifier
              eat(LPAREN, RPAREN)
              New(id).setPos(pos)
            }
          }
          case _ => expected(BANG, BANG)
        })
      }

      def termRest(lhs: ExprTree): ExprTree = {
        val pos = currentToken
        var e = lhs
        while (currentToken.kind == DOT || currentToken.kind == LBRACKET) {
          if (currentToken.kind == DOT) {
            eat(DOT)
            if (currentToken.kind == LENGTH) {
              eat(LENGTH)
              e = ArrayLength(e).setPos(pos)
            } else {
              val id = identifier
              eat(LPAREN)
              val exprs = commaList(expression)
              eat(RPAREN)
              e = MethodCall(e, id, exprs.toList).setPos(pos)
            }
          } else if (currentToken.kind == LBRACKET) {
            eat(LBRACKET)
            var expr = expression
            eat(RBRACKET)
            e = ArrayRead(e, expr).setPos(pos)
          }
        }
        e
      }

      or().setPos(pos)
    }

    def identifier(): Identifier = {
      try {
        val id = currentToken.asInstanceOf[ID]
        val t = currentToken
        eat(IDKIND)
        Identifier(id.value).setPos(t)
      } catch {
        case _: Throwable => expected(IDKIND)
      }
    }

    def stringLit(): StringLit = {
      try {
        val id = currentToken.asInstanceOf[STRLIT]
        val t = currentToken
        eat(STRLITKIND)
        StringLit(id.value).setPos(t)
      } catch {
        case _: Throwable => expected(STRLITKIND)
      }
    }

    def intLit(): IntLit = {
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
