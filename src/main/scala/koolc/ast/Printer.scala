package koolc
package ast

import Trees._

object Printer {

  def apply(t: Tree): String = t match {
    case Program(main, classes) => apply(main) + all(classes)
    case MainObject(id, stats)  => "object " + id.value + " { def main () : Unit = { " + all(stats) + "}}"
    case ClassDecl(id, parent, vars, methods) =>
      "class " + id.value + optional(parent, " extends " + _.asInstanceOf[Identifier].value) +
        " { " + all(vars) + all(methods) + "}"
    case VarDecl(tpe, id) => "var " + id.value + " : " + apply(tpe) + " ;"
    case MethodDecl(retType, id, args, vars, stats, retExpr) =>
      "def " + id.value + " ( " + commaList(args) + " ): " + apply(retType) +
        " = { " + all(vars) + all(stats) + " return " + apply(retExpr) + " ; }"
    case Formal(tpe, id)              => id.value + ": " + apply(tpe)
    // Types
    case IntType()                    => "Int"
    case IntArrayType()               => "Int[]"
    case BooleanType()                => "Bool"
    case StringType()                 => "String"
    // Statements
    case Block(stats)                 => "{ " + all(stats) + " }"
    case If(expr, thn, els)           => "if(" + apply(expr) + ")" + apply(thn) + optional(els, "else " + apply(_))
    case While(expr, stat)            => "while(" + apply(expr) + ")" + apply(stat)
    case Println(expr)                => "println(" + apply(expr) + ");"
    case Assign(id, expr)             => id.value + " = " + apply(expr) + ";"
    case ArrayAssign(id, index, expr) => id.value + "[" + apply(index) + "] = " + apply(expr) + ";"
    // Expressions
    case And(lhs, rhs)                => apply(lhs) + " && " + apply(rhs)
    case Or(lhs, rhs)                 => apply(lhs) + " || " + apply(rhs)
    case Plus(lhs, rhs)               => apply(lhs) + " + " + apply(rhs)
    case Minus(lhs, rhs)              => apply(lhs) + " - " + apply(rhs)
    case Times(lhs, rhs)              => apply(lhs) + " * " + apply(rhs)
    case Div(lhs, rhs)                => apply(lhs) + " / " + apply(rhs)
    case LessThan(lhs, rhs)           => apply(lhs) + " < " + apply(rhs)
    case Equals(lhs, rhs)             => apply(lhs) + " == " + apply(rhs)
    case ArrayRead(arr, index)        => apply(arr) + "[" + apply(index) + "]"
    case ArrayLength(arr)             => apply(arr) + ".length"
    case MethodCall(obj, meth, args)  => apply(obj) + "." + meth.value + "(" + commaList(args) + ")"
    case IntLit(value)                => value.toString
    case StringLit(value)             => "\"" + value + "\""
    case True()                       => "true"
    case False()                      => "false"
    case Identifier(value)            => value
    case This()                       => "this"
    case NewIntArray(size)            => "new Int[" + apply(size) + "]"
    case New(tpe)                     => "new " + tpe.value + "()"
    case Not(expr)                    => "!" + apply(expr)
  }

  private def optional(t: Option[Tree], f: (Tree) => String) = if (t.isDefined) f(t.get) else "" 
  
  private def commaList(list: List[Tree]): String = {
    val s = list.foldLeft("")(_ + apply(_) + ", ")
    s.substring(0, s.length - 1)
  }

  private def all(list: List[Tree], start: String = ""): String = list.foldLeft(start)(_ + apply(_))
}
