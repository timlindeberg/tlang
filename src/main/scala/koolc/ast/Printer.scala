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
      if (t.hasSymbol) "[" + t.getSymbol.getType + "]" else "??"
    else ""

  def apply(t: Tree, printIdNumber: Boolean = false): String = {
    this.printIdNumber = printIdNumber
    f(t)
  }

  private def f(t: Tree): String = {
    val s = t match {
      case Program(main, classes) => f(main) + all(classes)
      case MainObject(id, stats) => "object " + f(id) + " " + l + "def main () : Unit = " + l + all(stats) + r + r
      case ClassDecl(id, parent, vars, methods) => n + n + "class " + f(id) + optional(parent, t => " extends " + f(t.asInstanceOf[TypeIdentifier])) + " " + l + all(vars) + all(methods) + "" + r
      case VarDecl(tpe, id) => "var " + f(id) + " : " + f(tpe) + ";" + n
      case MethodDecl(retType, id, args, vars, stats, retExpr) => "def " + f(id) + " (" + commaList(args) + ") : " + f(retType) + " = " + l + all(vars) + all(stats) + "return " + f(retExpr) + "; " + r + n
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
      case MethodCall(obj, meth, args) => f(obj) + "." + f(meth) + "(" + commaList(args) + ")"
      case IntLit(value) => value.toString
      case StringLit(value) => "\"" + value + "\""
      case True() => "true"
      case False() => "false"
      case id @ Identifier(value) => value + symbol(id)
      case id @ TypeIdentifier(value, list) => value + symbol(id) + (if (id.isTemplated) "[" + commaList(list) + "]" else "")
      case This() => "this"
      case NewIntArray(size) => "new Int[" + f(size) + "]"
      case New(tpe) => "new " + f(tpe) + "()"
      case Not(expr) => "!(" + f(expr) + ")"
    }
    s
  }

  private def pos(t: Tree): String = "[" + t.line + ", " + t.col + "]"

  private def optional(t: Option[Tree], f: (Tree) => String) = if (t.isDefined) f(t.get) else ""

  private def commaList(list: List[Tree]): String = list.map(f).mkString(", ")

  private def all(list: List[Tree], start: String = "") = list.foldLeft(start)(_ + f(_))
}
