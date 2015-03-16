package koolc
package ast

import Trees._

object Printer {
  def apply(t: Tree): String = {
    t match {
      case Program(main, classes) => apply(main); classes.foreach(apply);
      case MainObject(id, stats) => {
        print("object " + id.value + " { def main () : Unit = { ")
        stats.foreach(apply)
        print("}}")
        println
      }
      case ClassDecl(id, parent, vars, methods) => {
        print("class " + id.value)
        if (parent.isDefined) {
          print(" extends " + parent.get.value)
        }
        print(" { ")
        vars.foreach(apply)
        methods.foreach(apply)
        print("}")
        println
      }
      case VarDecl(tpe, id) => {
        print("var " + id.value + " : ")
        apply(tpe)
        print(" ;")
      }
      case MethodDecl(retType, id, args, vars, stats, retExpr) => {
        print("def " + id.value + " ( ")
        args.foreach(apply)
        print(" ): ")
        apply(retType)
        print(" = { ")
        vars.foreach(apply)
        stats.foreach(apply)
        print(" return ")
        apply(retExpr)
        print(" ; }")

      }
      // Types
      case IntType()                    => print("Int")
      case IntArrayType()               => print("Int[]")
      case BooleanType()                => print("Bool")
      case StringType()                 => print("String")
      // Statements
      case Block(stats)                 => ???
      case If(expr, thn, els)           => ???
      case While(expr, stat)            => ???
      case Println(expr)                => ???
      case Assign(id, expr)             => ???
      case ArrayAssign(id, index, expr) => ???
      // Expressions
      case And(lhs, rhs)                => ???
      case Or(lhs, rhs)                 => ???
      case Plus(lhs, rhs)               => ???
      case Minus(lhs, rhs)              => ???
      case Times(lhs, rhs)              => ???
      case Div(lhs, rhs)                => ???
      case LessThan(lhs, rhs)           => ???
      case Equals(lhs, rhs)             => ???
      case ArrayRead(arr, index)        => ???
      case ArrayLength(arr)             => ???
      case MethodCall(obj, meth, args)  => ???
      case IntLit(value)                => ???
      case StringLit(value)             => ???
      case True()                       => ???
      case False()                      => ???
      case Identifier(value)            => ???
      case This()                       => ???
      case NewIntArray(size)            => ???
      case New(tpe)                     => ???
      case Not(expr)                    => ???
    }
  }
}
