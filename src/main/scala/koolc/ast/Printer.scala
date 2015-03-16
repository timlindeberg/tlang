package koolc
package ast

import Trees._

object Printer {

  def apply(t: Tree): String = {
    t match {
      case Program(main, classes) => apply(main) + classes.foldLeft("")(_ + apply(_))
      case MainObject(id, stats) => {
        var s = "object " + id.value + " { def main () : Unit = { "
        stats.foldLeft(s)(_ + apply(_))
        s + "}}"
      }
      case ClassDecl(id, parent, vars, methods) => {
        var s = "class " + id.value
        if (parent.isDefined) {
          s += " extends " + parent.get.value
        }
        s += " { "
        vars.foldLeft(s)(_ + apply(_))
        methods.foldLeft(s)(_ + apply(_))
        s + "}"
      }
      case VarDecl(tpe, id) => "var " + id.value + " : " + apply(tpe) + " ;"
      case MethodDecl(retType, id, args, vars, stats, retExpr) => {
        var s = "def " + id.value + " ( "
        s = args.foldLeft(s)(_ + apply(_))
        s += " ): " + apply(retType) + " = { "
        vars.foldLeft(s)(_ + apply(_))
        stats.foldLeft(s)(_ + apply(_))
        s += " return " + apply(retExpr)
        s + " ; }"
      }
      // Types
      case IntType()                    => "Int"
      case IntArrayType()               => "Int[]"
      case BooleanType()                => "Bool"
      case StringType()                 => "String"
      // Statements
      case Block(stats)                 => "{ " + stats.foldLeft("")(_ + apply(_)) + " }"
      case If(expr, thn, els)           => "if(" + apply(expr) + ")" + apply(thn) + (if (els.isDefined) "else " + apply(els.get) else "")
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
      case MethodCall(obj, meth, args) => {
        val argss = args.foldLeft("")(_ + apply(_) + ",")
        apply(obj) + "." + meth.value + "(" + argss.substring(0, argss.length - 1) + ")"
      }
      case IntLit(value)     => value.toString
      case StringLit(value)  => "\"" + value + "\""
      case True()            => "true"
      case False()           => "false"
      case Identifier(value) => value
      case This()            => "this"
      case NewIntArray(size) => "new Int[" + apply(size) + "]"
      case New(tpe)          => "new " + tpe.value + "()"
      case Not(expr)         => "!" + apply(expr)
    }
  }
}
