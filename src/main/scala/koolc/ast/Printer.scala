package koolc
package ast

import Trees._
import koolc.analyzer.Symbols._

object Printer {

  var indent: Int = 0
  var printIdNumber = false

  def l: String = {
    indent += 1
    "{" + n
  }

  def r: String = {
    indent -= 1
    n + "}"
  }

  def n: String = "\n" + " " * (2 * indent)
 

  def symbol[T <: Symbol](t: Symbolic[T]): String = 
    if (printIdNumber) 
      "#" + (if(t.hasSymbol) t.getSymbol.id else "??")
    else ""

  def apply(t: Tree, printIdNumber: Boolean = false): String = {
    this.printIdNumber = printIdNumber
    f(t)
  }

  private def f(t: Tree): String = t match {
    case Program(main, classes) => f(main) + all(classes)
    case MainObject(id, stats) => "object " + f(id) + " " + l + "def main () : Unit = " + l + all(stats) + r + r
    case ClassDecl(id, parent, vars, methods) => n + n + "class " + f(id) + optional(parent, t => " extends " + f(t.asInstanceOf[Identifier])) + " " + l + all(vars) + all(methods) + "" + r
    case VarDecl(tpe, id) => "var " + f(id) + " : " + f(tpe) + ";" + n
    case MethodDecl(retType, id, args, vars, stats, retExpr) => "def " + f(id) + " ( " + commaList(args) + " ) : " + f(retType) + " = " + l + all(vars) + all(stats) + "return " + f(retExpr) + "; " + r + n
    case Formal(tpe, id) => f(id) + ": " + f(tpe)

    // Types
    case IntType() => "Int"
    case IntArrayType() => "Int[]"
    case BooleanType() => "Bool"
    case StringType() => "String"
    // Statements
    case Block(stats) => l + all(stats) + r
    case If(expr, thn, els) => "if(" + f(expr) + ")" + f(thn) + optional(els, "else " + f(_)) + n
    case While(expr, stat) => "while(" + f(expr) + ")" + f(stat) + n
    case Println(expr) => "println(" + f(expr) + "); " + n
    case Assign(id, expr) => id.value + " = " + f(expr) + "; " + n
    case ArrayAssign(id, index, expr) => f(id) + "[" + f(index) + "] = " + f(expr) + ";" + n
    // Expressions
    case And(lhs, rhs) => "(" + f(lhs) + " && " + f(rhs) + ")"
    case Or(lhs, rhs) => "(" + f(lhs) + " || " + f(rhs) + ")"
    case Plus(lhs, rhs) => "(" + f(lhs) + " + " + f(rhs) + ")"
    case Minus(lhs, rhs) => "(" + f(lhs) + " - " + f(rhs) + ")"
    case Times(lhs, rhs) => "(" + f(lhs) + " * " + f(rhs) + ")"
    case Div(lhs, rhs) => "(" + f(lhs) + " / " + f(rhs) + ")"
    case LessThan(lhs, rhs) => "(" + f(lhs) + " < " + f(rhs) + ")"
    case Equals(lhs, rhs) => "(" + f(lhs) + " == " + f(rhs) + ")"
    case ArrayRead(arr, index) => f(arr) + "[" + f(index) + "]"
    case ArrayLength(arr) => f(arr) + ".length"
    case MethodCall(obj, meth, args) => f(obj) + "." + apply(meth) + "(" + commaList(args) + ")"
    case IntLit(value) => value.toString
    case StringLit(value) => "\"" + value + "\""
    case True() => "true"
    case False() => "false"
    case x @ Identifier(value) => value + symbol(x)
    case This() => "this"
    case NewIntArray(size) => "new Int[" + f(size) + "]"
    case New(tpe) => "new " + f(tpe) + "()"
    case Not(expr) => "!(" + f(expr) + ")"
  }

  def positionTree(t: Tree): String = {

    def all(list: List[Tree], indent: Int) = list.foldLeft("")(_ + "," + pt(_, indent))

    def pt(t: Tree, indent: Int): String = {
      val i = indent + 1
      val s = t match {
        case Program(main, classes) => pt(main, i) + all(classes, i)
        case MainObject(id, stats) => pt(id, i) + all(stats, i)
        case ClassDecl(id, parent, vars, methods) => (if (parent.isDefined) pt(parent.get, i) else "") + all(vars, i) + all(methods, i)
        case VarDecl(tpe, id) => pt(tpe, i) + pt(id, i)
        case MethodDecl(retType, id, args, vars, stats, retExpr) => pt(id, i) + all(args, i) + all(vars, i) + all(stats, i) + pt(retExpr, i)
        case Formal(tpe, id) => pt(tpe, i) + pt(id, i)
        // Statements
        case Block(stats) => all(stats, i)
        case If(expr, thn, els) => pt(expr, i) + pt(thn, i) + (if (els.isDefined) pt(els.get, i) else "")
        case While(expr, stat) => pt(expr, i) + pt(stat, i)
        case Println(expr) => pt(expr, i)
        case Assign(id, expr) => pt(id, i) + pt(expr, i)
        case ArrayAssign(id, index, expr) => pt(id, i) + pt(index, i) + pt(expr, i)
        // Expressions
        case And(lhs, rhs) => pt(lhs, i) + pt(rhs, i)
        case Or(lhs, rhs) => pt(lhs, i) + pt(rhs, i)
        case Plus(lhs, rhs) => pt(lhs, i) + pt(rhs, i)
        case Minus(lhs, rhs) => pt(lhs, i) + pt(rhs, i)
        case Times(lhs, rhs) => pt(lhs, i) + pt(rhs, i)
        case Div(lhs, rhs) => pt(lhs, i) + pt(rhs, i)
        case LessThan(lhs, rhs) => pt(lhs, i) + pt(rhs, i)
        case Equals(lhs, rhs) => pt(lhs, i) + pt(rhs, i)
        case ArrayRead(arr, index) => pt(arr, i) + pt(index, i)
        case ArrayLength(arr) => pt(arr, i)
        case MethodCall(obj, meth, args) => pt(obj, i) + pt(meth, i) + all(args, i)
        case IntLit(value) => value
        case StringLit(value) => value
        case Identifier(value) => value
        case NewIntArray(size) => pt(size, i)
        case New(tpe) => pt(tpe, i)
        case Not(expr) => pt(expr, i)
        case _ => ""
      }
      "\n" + (" " * 2 * i) + t.getClass.getSimpleName + pos(t) + "(" + s + ")"
    }
    pt(t, 1)
  }

  private def pos(t: Tree): String = "[" + t.line + ", " + t.col + "]"

  private def optional(t: Option[Tree], f: (Tree) => String) = if (t.isDefined) f(t.get) else ""

  private def commaList(list: List[Tree]): String =
    if (list.size <= 0) {
      ""
    } else {
      val s = list.foldLeft("")(_ + f(_) + ", ")
      s.substring(0, s.length - 2) // remove last comma
    }

  private def all(list: List[Tree], start: String = "") = list.foldLeft(start)(_ + f(_))
}
