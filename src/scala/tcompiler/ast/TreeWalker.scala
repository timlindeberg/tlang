package tcompiler.ast

import tcompiler.ast.Trees._

/**
  * Created by Tim Lindeberg on 5/18/2016.
  */
object TreeWalker {

  type TreeMod = Tree => Option[Tree]

  def traverse(t: Tree, f: (Tree, Tree) => Any): Unit = {
    def trav(parent: Tree, current: Tree): Unit = {
      current.productIterator.foreach {
        case x: Iterable[_] =>
          x.foreach(Some(_) collect { case x: Tree => trav(current, x) })
        case x: Option[Any] =>
          x collect { case x: Tree => trav(current, x) }
        case x: Tree        => trav(current, x)
        case _              =>
      }
      f(parent, current)
    }
    trav(t, t)
  }

  def modify(t: Tree, f: TreeMod): Tree = {

    //def modify(l: List[Tree]): List[Tree] = l.map(modify)
    /*def modify(maybeTree: Option[Tree]): Option[Tree] = {
      maybeTree match {
        case Some(t) => Some(modify(t))
        case None    => None

      }
    }*/


    def modify[T <: Tree](t: T): Tree = {
      t match {
        case Program(progPackage, imports, classes, importMap)              =>
        case Package(identifiers)                                           =>
        case RegularImport(identifiers)                                     =>
        case WildCardImport(identifiers)                                    =>
        case TemplateImport(identifiers)                                    =>
        case ClassDecl(id, parents, fields, methods, isTrait)               =>
        case MethodDecl(retType, id, args, stat, modifiers)                 =>
        case ConstructorDecl(retType, id, args, stat, modifiers)            =>
        case OperatorDecl(operatorType, retType, args, stat, modifiers, id) =>

        case Formal(tpe, id) =>

        case Public()    =>
        case Private()   =>
        case Protected() =>
        case Static()    =>
        case Implicit()  =>
        case Final()     =>

        case ArrayType(tpe)    =>
        case NullableType(tpe) =>
        case IntType()         =>
        case LongType()        =>
        case FloatType()       =>
        case DoubleType()      =>
        case BooleanType()     =>
        case CharType()        =>
        case StringType()      =>
        case UnitType()        =>

        case VarDecl(tpe, id, init, modifiers) =>
        case Block(stats)                      =>
        case If(expr, thn, els)                =>
        case While(expr, stat)                 =>
        case For(init, condition, post, stat)  =>
        case Foreach(varDecl, container, stat) =>
        case Print(expr)                       =>
        case Println(expr)                     =>
        case Error(expr)                       =>
        case Return(expr)                      =>
        case Break()                           =>
        case Continue()                        =>

        case Assign(id, expr)              =>
        case ArrayAssign(arr, index, expr) =>
        case FieldRead(obj, id)            =>
        case FieldAssign(obj, id, expr)    =>

        case And(lhs, rhs)        =>
        case Or(lhs, rhs)         =>
        case LogicAnd(lhs, rhs)   =>
        case LogicOr(lhs, rhs)    =>
        case LogicXor(lhs, rhs)   =>
        case Plus(lhs, rhs)       =>
        case Minus(lhs, rhs)      =>
        case LeftShift(lhs, rhs)  =>
        case RightShift(lhs, rhs) =>
        case Times(lhs, rhs)      =>
        case Div(lhs, rhs)        =>
        case Modulo(lhs, rhs)     =>

        case LessThan(lhs, rhs)          =>
        case LessThanEquals(lhs, rhs)    =>
        case GreaterThan(lhs, rhs)       =>
        case GreaterThanEquals(lhs, rhs) =>

        case Instance(expr, id)          =>
        case As(expr, tpe)               =>
        case Equals(lhs, rhs)            =>
        case NotEquals(lhs, rhs)         =>
        case ArrayRead(arr, index)       =>
        case ArraySlice(arr, start, end) =>
        case MethodCall(obj, meth, args) =>
        case IntLit(value)               =>
        case LongLit(value)              =>
        case FloatLit(value)             =>
        case DoubleLit(value)            =>
        case CharLit(value)              =>
        case StringLit(value)            =>
        case ArrayLit(expressions)       =>
        case True()                      =>
        case False()                     =>
        case Null()                      =>

        case Identifier(value)                     =>
        case ClassIdentifier(value, templateTypes) =>
        case This()                                =>
        case Super(specifier)                      =>
        case NewArray(tpe, sizes)                  =>
        case New(tpe, args)                        =>
        case Not(expr)                             =>
        case Hash(expr)                            =>
        case Negation(expr)                        =>
        case LogicNot(expr)                        =>
        case PreIncrement(id)                      =>
        case PostIncrement(id)                     =>
        case PreDecrement(id)                      =>
        case PostDecrement(id)                     =>
        case Ternary(condition, thn, els)          =>
        case Empty()                               =>
      }
      f(t) match {
        case Some(newTree) => newTree
        case None          => t
      }
    }
    modify(t)
  }

}
