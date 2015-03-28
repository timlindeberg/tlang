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
    case And(lhs, rhs) => "(" + apply(lhs) + " && " + apply(rhs) + ")"
    case Or(lhs, rhs) => "(" + apply(lhs) + " || " + apply(rhs) + ")"
    case Plus(lhs, rhs) => "(" + apply(lhs) + " + " + apply(rhs) + ")"
    case Minus(lhs, rhs) => "(" + apply(lhs) + " - " + apply(rhs) + ")"
    case Times(lhs, rhs) => "(" + apply(lhs) + " * " + apply(rhs) + ")"
    case Div(lhs, rhs) => "(" + apply(lhs) + " / " + apply(rhs) + ")"
    case LessThan(lhs, rhs) => "(" + apply(lhs) + " < " + apply(rhs) + ")"
    case Equals(lhs, rhs) => "(" + apply(lhs) + " == " + apply(rhs) + ")"
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
    case Not(expr) => "!(" + apply(expr) + ")"
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
