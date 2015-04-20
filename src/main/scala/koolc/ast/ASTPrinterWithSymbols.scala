package koolc
package ast

import Trees._
import analyzer.Symbols._
import analyzer.Types._

object ASTPrinterWithSymbols {

  def apply(t: Tree): String = f(t)

  private def f(t: Tree): String = {
    val s = t match {
      case Program(main, classes) => f(main) + "," + trees(classes)
      case MainObject(id, stats) => f(id) + "," + trees(stats)
      case ClassDecl(id, parent, vars, methods) => f(id) + "," + optional(parent) + "," + trees(vars) + "," + trees(methods)
      case VarDecl(tpe, id) => f(tpe) + "," + f(id)
      case MethodDecl(retType, id, args, vars, stats, retExpr) => f(retType) + "," + f(id) + "," + trees(args) + "," + trees(vars) + "," + trees(stats) + "," + f(retExpr)
      case Formal(tpe, id) => f(tpe) + "," + f(id)
      case Block(stats) => trees(stats)
      case If(expr, thn, els) => f(expr) + "," + f(thn) + "," + optional(els)
      case While(expr, stat) => f(expr) + "," + f(stat)
      case Println(expr) => f(expr)
      case Assign(id, expr) => f(id) + "," + f(expr)
      case ArrayAssign(id, index, expr) => f(id) + "," + f(index) + "," + f(expr)
      case And(lhs, rhs) => f(lhs) + "," + f(rhs)
      case Or(lhs, rhs) => f(lhs) + "," + f(rhs)
      case Plus(lhs, rhs) => f(lhs) + "," + f(rhs)
      case Minus(lhs, rhs) => f(lhs) + "," + f(rhs)
      case Times(lhs, rhs) => f(lhs) + "," + f(rhs)
      case Div(lhs, rhs) => f(lhs) + "," + f(rhs)
      case LessThan(lhs, rhs) => f(lhs) + "," + f(rhs)
      case Equals(lhs, rhs) => f(lhs) + "," + f(rhs)
      case ArrayRead(arr, index) => f(arr) + "," + f(index)
      case ArrayLength(arr) => f(arr)
      case MethodCall(obj, meth, args) => f(obj) + "," + f(meth) + "," + trees(args)
      case IntLit(value) => value
      case StringLit(value) => value
      case Identifier(value) => value
      case NewIntArray(size) => f(size)
      case New(tpe) => f(tpe)
      case Not(expr) => f(expr)
      case _ => ""
    }
    t.getClass.getSimpleName + symbol(t) + "(" + s + ")"
  }

  def typeOf(t: Tree): String = t match {
    case typed: Typed => "[" + typed.getType + "]"
    case _            => ""
  }

  def symbol(t: Tree): String = t match {
    case sym: Symbolic[_] if sym.hasSymbol => "#" + sym.getSymbol.id
    case _                                 => ""
  }

  private def optional(t: Option[Tree]) = t match { case Some(p) => "Some(" + f(p) + ")" case None => "None" }
  private def trees(trees: List[Tree]) = "List(" + trees.map(f).mkString(",") + ")"

}