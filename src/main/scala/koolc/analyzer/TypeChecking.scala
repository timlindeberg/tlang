package koolc
package analyzer

import koolc.analyzer.Types._
import koolc.ast.Trees._
import koolc.utils._
import koolc.ast.Printer

object TypeChecking extends Pipeline[Program, Program] {

  /**
   * Typechecking does not produce a value, but has the side effect of
   * attaching types to trees and potentially outputting error messages.
   */
  def run(ctx: Context)(prog: Program): Program = {

    def typeCheckMethodCall(mc: MethodCall) = {
      val obj = mc.obj
      val meth = mc.meth
      val methodCallArgs = mc.args
      def methodSignature = meth.value + methodCallArgs.map(_.getType).mkString("(", ", ", ")")

      val objType = tcExpr(obj)
      val argTypes = methodCallArgs.map(tcExpr(_))

      objType match {
        case TObject(classSymbol) =>
          classSymbol.lookupMethod(meth.value, argTypes) match {
            case Some(methodSymbol) =>
                meth.setSymbol(methodSymbol)
                meth.getType
            case None =>
              error("Class \'" + classSymbol.name + "\' does not contain a method \'" + methodSignature + "\'.", mc)
          }
        case _ => error("Cannot call function on type " + objType, mc)
      }
    }

    def tcStat(stat: StatTree, retType: Type): Unit = stat match {
      case Block(stats) => stats.foreach(tcStat(_, retType))
      case If(expr, thn, els) =>
        tcExpr(expr, TBool)
        tcStat(thn, retType)
        if (els.isDefined)
          tcStat(els.get, retType)
      case While(expr, stat) =>
        tcExpr(expr, TBool)
        tcStat(stat, retType)
      case For(init, condition, post, stat) =>
        init.foreach(tcStat(_, retType))
        tcExpr(condition, TBool)
        post.foreach(tcStat(_, retType))
        tcStat(stat, retType)
      case Println(expr)    =>
        if(tcExpr(expr) == TUnit) error("Type error: cannot call println with type Unit!", expr)
      case Assign(id, expr) => tcExpr(expr, id.getType)
      case Assignment(id, expr) =>
        tcExpr(id, TInt)
        tcExpr(expr, TInt)
      case ArrayAssign(id, index, expr) =>
        tcExpr(index, TInt)
        tcExpr(expr, TInt)
      case mc: MethodCall => mc.setType(typeCheckMethodCall(mc))
      case Return(Some(expr)) =>
        tcExpr(expr, retType)
      case r @ Return(None) =>
        if(retType != TUnit) error("Expected a return value of type \'" + retType + "\'.", r)
      case IncrementDecrement(id) =>
        tcExpr(id, TInt)
    }

    def tcExpr(expr: ExprTree, expected: Type*): Type = {
      tcExprList(expr, expected.toList)
    }

    def tcExprList(expr: ExprTree, expected: List[Type]): Type = {
      val tpe = expr match {
        case And(lhs, rhs) =>
          tcExpr(lhs, TBool)
          tcExpr(rhs, TBool)
          TBool
        case Or(lhs, rhs) =>
          tcExpr(lhs, TBool)
          tcExpr(rhs, TBool)
          TBool
        case Equals(lhs, rhs) =>
          (tcExpr(lhs), tcExpr(rhs)) match {
            case (TObject(_), TObject(_)) => TBool
            case (x, y) if x == y         => TBool
            case (x, y)                   => error("Type error: expected: " + x + ", found " + y, rhs)
          }
        case NotEquals(lhs, rhs) =>
          (tcExpr(lhs), tcExpr(rhs)) match {
            case (TObject(_), TObject(_)) => TBool
            case (x, y) if x == y         => TBool
            case (x, y)                   => error("Type error: expected: " + x + ", found " + y, rhs)
          }
        case Instance(expr, id) =>
          val tpe = tcExpr(expr)
          if(!tpe.isInstanceOf[TObject])
            error("Type error: expected: object , found " + tpe, expr)
          tcExprList(id, tpe.getSuperTypes)
          TBool
        case As(expr, tpe) =>
          tcExprList(expr, tpe.getType.getSuperTypes)
          tpe.getType
        case Comparison(lhs, rhs) =>
          tcExpr(lhs, TInt)
          tcExpr(rhs, TInt)
          TBool
        case Plus(lhs, rhs) =>
          (tcExpr(lhs, TInt, TString), tcExpr(rhs, TInt, TString)) match {
            case (TInt, TInt) => TInt
            case _            => TString
          }
        case MathBinaryExpr(lhs, rhs) =>
          tcExpr(lhs, TInt)
          tcExpr(rhs, TInt)
          TInt
        case ArrayRead(arr, index) =>
          tcExpr(arr, TIntArray)
          tcExpr(index, TInt)
          TInt
        case ArrayLength(arr) =>
          tcExpr(arr, TIntArray)
          TInt
        case mc : MethodCall        => typeCheckMethodCall(mc)
        case IntLit(value)          => TInt
        case StringLit(value)       => TString
        case True()                 => TBool
        case False()                => TBool
        case id @ Identifier(value) => id.getType
        case id @ TypeIdentifier(value, _) => id.getType
        case th @ This()            => th.getSymbol.getType
        case NewIntArray(size) =>
          tcExpr(size, TInt)
          TIntArray
        case n @ New(tpe, exprs) =>
          val argTypes = exprs.map(tcExpr(_))

          tpe.getType match {
            case TObject(classSymbol) =>
              classSymbol.lookupMethod(tpe.value, argTypes) match {
                case Some(_) =>
                case None =>
                  if(exprs.size > 0){
                    val methodSignature = tpe.value + exprs.map(_.getType).mkString("(", " , ", ")")
                    error("Class \'" + classSymbol.name + "\' does not contain a constructor \'" + methodSignature + "\'.", n)
                  }
              }
            case x => error("Cannot create a new instance of primitive type \'" + x + "\'.", n)
          }
          tpe.getType
        case Not(expr) =>
          tcExpr(expr, TBool)
          TBool
        case Negation(expr) =>
          tcExpr(expr, TInt)
          TInt
        case IncrementDecrement(id) =>
          tcExpr(id, TInt)
          TInt
        case LogicNot(expr)  =>
          tcExpr(expr, TInt)
          TInt
        case Ternary(condition, thn, els) =>
          tcExpr(condition, TBool)
          val tpe = tcExpr(thn)
          tcExpr(els, tpe)
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
    prog.main.stats.foreach(tcStat(_, TUnit))
    prog.classes.foreach(_.methods.foreach(_ match {
      case method: MethodDecl =>
        method.stats.foreach(tcStat(_,method.getSymbol.getType))
      case constructor: ConstructorDecl =>
        constructor.stats.foreach(tcStat(_, TUnit))
    }))

    prog
  }



}
