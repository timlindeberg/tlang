package koolc
package ast

import Trees._
import analyzer.Symbols._
import analyzer.Types._

object ASTPrinterWithSymbols {

  def apply(t: Tree): String = f(t)

  private def f(t: Tree): String = {
    val s = t match {
      case Program(pack, imports, main, classes) => f(pack) + "," + f(imports) + "," + f(main) + "," + f(classes)
      case Package(identifiers) => f(identifiers)
      case RegularImport(identifiers) => f(identifiers)
      case WildCardImport(identifiers) => f(identifiers)
      case GenericImport(identifiers) => f(identifiers)
      case MainObject(id, stats) => f(id) + "," + f(stats)
      case ClassDecl(id, parent, vars, methods) => f(id) + "," + f(parent) + "," + f(vars) + "," + f(methods)
      case VarDecl(tpe, id, init) => f(tpe) + "," + f(id) + "," + init
      case MethodDecl(retType, id, args, vars, stats, access) => f(retType) + "," + f(id) + "," + f(args) + "," + f(vars) + "," + f(stats) + "," + f(access)
      case Formal(tpe, id) => f(tpe) + "," + f(id)
      case Block(stats) => f(stats)
      case If(expr, thn, els) => f(expr) + "," + f(thn) + "," + f(els)
      case While(expr, stat) => f(expr) + "," + f(stat)
      case For(init, condition, post, stat) => f(init) + "," + f(condition) + "," + f(post) + "," + f(stat)
      case Println(expr) => f(expr)
      case Assign(id, expr) => f(id) + "," + f(expr)
      case ArrayAssign(id, index, expr) => f(id) + "," + f(index) + "," + f(expr)
      case Return(expr) => f(expr)
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
      case MethodCall(obj, meth, args) => f(obj) + "," + f(meth) + "," + f(args)
      case IntLit(value) => value
      case StringLit(value) => value
      case Identifier(value) => value
      case ClassIdentifier(value, templateTypes) => value + "," + f(templateTypes)
      case NewArray(tpe, size) => f(tpe) + "," + f(size)
      case New(tpe, exprs) => f(tpe) + ", " + f(exprs)
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

  private def f(t: Option[Tree]): String = t match { case Some(p) => "Some(" + f(p) + ")" case None => "None" }
  private def f(trees: List[Tree]): String = "List(" + trees.map(f).mkString(",") + ")"

}