package koolc
package ast

import Trees._

object Printer {

  var indent: Int = 0

  def l: String = {
    indent += 1
    "{" + n
  }

  def r: String = {
    indent -= 1
    n + "}"
  }

  def n: String = {
    "\n" + " " * (2 * indent)
  }

  def apply(t: Tree): String = t match {
    case Program(main, classes) => apply(main) + all(classes)
    case MainObject(id, stats) => "object " + id.value + " " + l + "def main () : Unit = " + l + all(stats) + r + r
    case ClassDecl(id, parent, vars, methods) => n + n + "class " + id.value + optional(parent, " extends " + _.asInstanceOf[Identifier].value) + " " + l + all(vars) + all(methods) + "" + r
    case VarDecl(tpe, id) => "var " + id.value + " : " + apply(tpe) + ";" + n
    case MethodDecl(retType, id, args, vars, stats, retExpr) => "def " + id.value + " ( " + commaList(args) + " ) : " + apply(retType) + " = " + l + all(vars) + all(stats) + "return " + apply(retExpr) + "; " + r + n
    case Formal(tpe, id) => id.value + ": " + apply(tpe)

    // Types
    case IntType() => "Int"
    case IntArrayType() => "Int[]"
    case BooleanType() => "Bool"
    case StringType() => "String"
    // Statements
    case Block(stats) => l + all(stats) + r
    case If(expr, thn, els) => "if(" + apply(expr) + ")" + apply(thn) + optional(els, "else " + apply(_)) + n
    case While(expr, stat) => "while(" + apply(expr) + ")" + apply(stat) + n
    case Println(expr) => "println(" + apply(expr) + "); " + n
    case Assign(id, expr) => id.value + " = " + apply(expr) + "; " + n
    case ArrayAssign(id, index, expr) => id.value + "[" + apply(index) + "] = " + apply(expr) + ";" + n
    // Expressions
    case And(lhs, rhs) => apply(lhs) + " && " + apply(rhs)
    case Or(lhs, rhs) => apply(lhs) + " || " + apply(rhs)
    case Plus(lhs, rhs) => apply(lhs) + " + " + apply(rhs)
    case Minus(lhs, rhs) => apply(lhs) + " - " + apply(rhs)
    case Times(lhs, rhs) => apply(lhs) + " * " + apply(rhs)
    case Div(lhs, rhs) => apply(lhs) + " / " + apply(rhs)
    case LessThan(lhs, rhs) => apply(lhs) + " < " + apply(rhs)
    case Equals(lhs, rhs) => apply(lhs) + " == " + apply(rhs)
    case ArrayRead(arr, index) => apply(arr) + "[" + apply(index) + "]"
    case ArrayLength(arr) => apply(arr) + ".length"
    case MethodCall(obj, meth, args) => apply(obj) + "." + meth.value + "(" + commaList(args) + ")"
    case IntLit(value) => value.toString
    case StringLit(value) => "\"" + value + "\""
    case True() => "true"
    case False() => "false"
    case Identifier(value) => value
    case This() => "this"
    case NewIntArray(size) => "new Int[" + apply(size) + "]"
    case New(tpe) => "new " + tpe.value + "()"
    case Not(expr) => "!" + apply(expr)
  }

  def positionTree(t: Tree): String = {

    def all(list: List[Tree], indent: Int) = list.foldLeft("")(_ + "," + pt(_, indent))

    def pt(t: Tree, indent: Int): String = {
      val i = indent + 1
      val s = " " * i
      val s2 = t match {
        case Program(main, classes) => "Program" + pos(t) + "(" + pt(main, i) + "," + all(classes, i) + ")"
        case MainObject(id, stats) => "MainObject" + pos(t) + "(" + pt(id, i) + "," + all(stats, i) + ")"
        case ClassDecl(id, parent, vars, methods) => "ClassDecl" + pos(t) + "(" + (if (parent.isDefined) pt(parent.get, i) else "") + all(vars, i) + all(methods, i) + ")"
        case VarDecl(tpe, id) => "VarDecl" + pos(t) + "(" + pt(tpe, i) + pt(id, i) + ")"
        case MethodDecl(retType, id, args, vars, stats, retExpr) => "MethodDecl" + pos(t) + "(" + pt(id, i) + all(args, i) + all(vars, i) + all(stats, i) + pt(retExpr, i) + ")"
        case Formal(tpe, id) => "Formal" + pos(t) + "(" + pt(tpe, i) + pt(id, i) + ")"
        // Types + ")"
        case IntType() => "IntType" + pos(t) + "()"
        case IntArrayType() => "IntArrayType" + pos(t) + "()"
        case BooleanType() => "BooleanType" + pos(t) + "()"
        case StringType() => "StringType" + pos(t) + "()"
        // Statements + ")"
        case Block(stats) => "Block" + pos(t) + "(" + all(stats, i) + ")"
        case If(expr, thn, els) => "If" + pos(t) + "(" + pt(expr, i) + pt(thn, i) + (if (els.isDefined) pt(els.get, i) else "") + ")"
        case While(expr, stat) => "While" + pos(t) + "(" + pt(expr, i) + pt(stat, i) + ")"
        case Println(expr) => "Println" + pos(t) + "(" + pt(expr, i) + ")"
        case Assign(id, expr) => "Assign" + pos(t) + "(" + pt(id, i) + pt(expr, i) + ")"
        case ArrayAssign(id, index, expr) => "ArrayAssign" + pos(t) + "(" + pt(id, i) + pt(index, i) + pt(expr, i) + ")"
        // Expressions + ")"
        case And(lhs, rhs) => "And" + pos(t) + "(" + pt(lhs, i) + pt(rhs, i) + ")"
        case Or(lhs, rhs) => "Or" + pos(t) + "(" + pt(lhs, i) + pt(rhs, i) + ")"
        case Plus(lhs, rhs) => "Plus" + pos(t) + "(" + pt(lhs, i) + pt(rhs, i) + ")"
        case Minus(lhs, rhs) => "Minus" + pos(t) + "(" + pt(lhs, i) + pt(rhs, i) + ")"
        case Times(lhs, rhs) => "Times" + pos(t) + "(" + pt(lhs, i) + pt(rhs, i) + ")"
        case Div(lhs, rhs) => "Div" + pos(t) + "(" + pt(lhs, i) + pt(rhs, i) + ")"
        case LessThan(lhs, rhs) => "LessThan" + pos(t) + "(" + pt(lhs, i) + pt(rhs, i) + ")"
        case Equals(lhs, rhs) => "Equals" + pos(t) + "(" + pt(lhs, i) + pt(rhs, i) + ")"
        case ArrayRead(arr, index) => "ArrayRead" + pos(t) + "(" + pt(arr, i) + pt(index, i) + ")"
        case ArrayLength(arr) => "ArrayLength" + pos(t) + "(" + pt(arr, i) + ")"
        case MethodCall(obj, meth, args) => "MethodCall" + pos(t) + "(" + pt(obj, i) + pt(meth, i) + all(args, i) + ")"
        case IntLit(value) => "IntLit" + pos(t) + "(" + value + ")"
        case StringLit(value) => "StringLit" + pos(t) + "(" + value + ")"
        case True() => "True" + pos(t) + "()"
        case False() => "False" + pos(t) + "()"
        case Identifier(value) => "Identifier" + pos(t) + "(" + value + ")"
        case This() => "This" + pos(t) + "()"
        case NewIntArray(size) => "NewIntArray" + pos(t) + "(" + pt(size, i) + ")"
        case New(tpe) => "New" + pos(t) + "(" + pt(tpe, i) + ")"
        case Not(expr) => "Not" + pos(t) + "(" + pt(expr, i) + ")"
      }
      "\n" + s + s2
    }
    pt(t, 1)
  }

  private def pos(t: Tree): String = "[" + t.line + ", " + t.col + "]"

  private def optional(t: Option[Tree], f: (Tree) => String) = if (t.isDefined) f(t.get) else ""

  private def commaList(list: List[Tree]): String = {
    if (list.size <= 0) {
      ""
    } else {
      val s = list.foldLeft("")(_ + apply(_) + ", ")
      s.substring(0, s.length - 2) // remove last comma
    }
  }

  private def all(list: List[Tree], start: String = "") = list.foldLeft(start)(_ + apply(_))
}
