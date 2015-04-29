package koolc
package analyzer

import ast.Trees._

import Symbols._
import Types._
import utils._

object TypeChecking extends Pipeline[Program, Program] {

  /**
   * Typechecking does not produce a value, but has the side effect of
   * attaching types to trees and potentially outputting error messages.
   */
  def run(ctx: Context)(prog: Program): Program = {
    import ctx.reporter

    def tcStat(stat: StatTree): Unit = stat match {
      case Block(stats) => stats.foreach(tcStat)
      case If(expr, thn, els) =>
        tcExpr(expr, TBool)
        tcStat(thn)
        if (els.isDefined)
          tcStat(els.get)
      case While(expr, stat) =>
        tcExpr(expr, TBool)
        tcStat(stat)
      case Println(expr)    => tcExpr(expr)
      case Assign(id, expr) => tcExpr(expr, id.getType)
      case ArrayAssign(id, index, expr) =>
        tcExpr(index, TInt)
        tcExpr(expr, TInt)
    }

    def tcExpr(expr: ExprTree, expected: Type*): Type = {
      val tpe = expr match {
        case And(lhs, rhs) =>
          tcExpr(lhs, TBool)
          tcExpr(rhs, TBool)
          TBool
        case Or(lhs, rhs) =>
          tcExpr(lhs, TBool)
          tcExpr(rhs, TBool)
          TBool
        case Plus(lhs, rhs) =>
          (tcExpr(lhs, TInt, TString), tcExpr(rhs, TInt, TString)) match {
            case (TInt, TInt) => TInt
            case _            => TString
          }
        case Minus(lhs, rhs) =>
          tcExpr(lhs, TInt)
          tcExpr(rhs, TInt)
          TInt
        case Times(lhs, rhs) =>
          tcExpr(lhs, TInt)
          tcExpr(rhs, TInt)
          TInt
        case Div(lhs, rhs) =>
          tcExpr(lhs, TInt)
          tcExpr(rhs, TInt)
          TInt
        case LessThan(lhs, rhs) =>
          tcExpr(lhs, TInt)
          tcExpr(rhs, TInt)
          TBool
        case eq @ Equals(lhs, rhs) =>
          (tcExpr(lhs), tcExpr(rhs)) match {
            case (TObject(_), TObject(_)) => TBool
            case (x, y) if x == y         => TBool
            case (x, y)                   => error("Type error: expected: " + x + ", found " + y, rhs)
          }
        case ArrayRead(arr, index) =>
          tcExpr(arr, TIntArray)
          tcExpr(index, TInt)
          TInt
        case ArrayLength(arr) =>
          tcExpr(arr, TIntArray)
          TInt
        case mc @ MethodCall(obj, meth, args) =>
          val objType = tcExpr(obj)
          objType match {
            case TObject(classSymbol) =>
              classSymbol.lookupMethod(meth) match {
                case Some(methodSymbol) =>
                  args.zip(methodSymbol.argList.map(_.getType)).foreach(x => tcExpr(x._1, x._2))
                  meth.setSymbol(methodSymbol)
                  meth.getType
                case None => error("Class \'" + classSymbol.name + "\' does not contain a method called \'" + meth.value + "\'.", mc)
              }
            case _ => error("Cannot call function on type " + objType, mc)
          }
        case IntLit(value)          => TInt
        case StringLit(value)       => TString
        case True()                 => TBool
        case False()                => TBool
        case id @ Identifier(value) => id.getType
        case th @ This()            => th.getSymbol.getType
        case NewIntArray(size) =>
          tcExpr(size, TInt)
          TIntArray
        case New(tpe)  => tpe.getType
        case Not(expr) => 
          tcExpr(expr)
          TBool
      }

      // Check result and return a valid type in case of error
      val res = if (expected.isEmpty) {
        tpe
      } else if (!expected.exists(e => tpe.isSubTypeOf(e))) {
        error("Type error: expected: " + expected.toList.mkString(" or ") + ", found: " + tpe, expr)
        expected.head
      } else {
        tpe
      }
      expr.setType(res)
      res
    }

    def error(msg: String, pos: Positioned) = {
      ctx.reporter.error(msg, pos)
      TError
    }

    prog.main.stats.foreach(tcStat)
    prog.classes.foreach(_.methods.foreach { method =>
      tcExpr(method.retExpr, method.getSymbol.getType)
      method.stats.foreach(tcStat)
    })

    prog
  }

}
