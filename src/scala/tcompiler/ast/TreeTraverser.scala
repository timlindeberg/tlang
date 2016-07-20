package tcompiler.ast

import tcompiler.ast.Trees.{ArrayType, _}
import tcompiler.utils.Extensions._

/**
  * Created by Tim Lindeberg on 5/18/2016.
  */
class TreeTraverser {

  def traverse(t: Tree): Unit = t match {
    case _: Leaf                                      => // No need to recurse further
    case CompilationUnit(pack, classes, _)            => traverse(pack, classes)
    case ClassDecl(id, parents, fields, methods, _)   => traverse(id, parents, fields, methods)
    case FuncTree(id, retType, args, stat, modifiers) => traverse(retType, id, args, stat, modifiers)
    case Formal(tpe, id)                              => traverse(tpe, id)
    case ArrayType(tpe)                               => traverse(tpe)
    case NullableType(tpe)                            => traverse(tpe)
    case PrintStatTree(expr)                          => traverse(expr)
    case VarDecl(tpe, id, init, modifiers)            => traverse(tpe, id, init, modifiers)
    case Block(stats)                                 => traverse(stats)
    case If(expr, thn, els)                           => traverse(expr, thn, els)
    case While(expr, stat)                            => traverse(expr, stat)
    case For(init, condition, post, stat)             => traverse(init, condition, post, stat)
    case Foreach(varDecl, container, stat)            => traverse(varDecl, container, stat)
    case Error(expr)                                  => traverse(expr)
    case Return(expr)                                 => traverse(expr)
    case BinaryOperatorTree(lhs, rhs)                 => traverse(lhs, rhs)
    case UnaryOperatorTree(expr)                      => traverse(expr)
    case Access(obj, application)                     => traverse(obj, application)
    case ArrayAssign(arr, index, expr)                => traverse(arr, index, expr)
    case ArrayRead(arr, index)                        => traverse(arr, index)
    case ArraySlice(arr, start, end)                  => traverse(arr, start, end)
    case ClassID(_, templateTypes)                    => traverse(templateTypes)
    case Assign(id, expr)                             => traverse(id, expr)
    case Super(specifier)                             => traverse(specifier)
    case NewArray(tpe, sizes)                         => traverse(tpe, sizes)
    case New(tpe, args)                               => traverse(tpe, args)
    case Ternary(condition, thn, els)                 => traverse(condition, thn, els)
    case Elvis(nullableValue, ifNull)                 => traverse(nullableValue, ifNull)
    case ExtractNullable(expr)                        => traverse(expr)
    case Is(expr, id)                                 => traverse(expr, id)
    case As(expr, tpe)                                => traverse(expr, tpe)
    case MethodCall(meth, args)                       => traverse(meth, args)
    case ArrayLit(value)                              => traverse(value)
    case GeneratedExpr(block)                         => traverse(block)
    case PutValue(expr)                               => traverse(expr)
  }

  def traverse(trees: Traversable[Tree]): Unit = trees foreach traverse

  def traverse(t: Any*): Unit = t foreach {
    case t: Tree              => traverse(t)
    case t: Traversable[Tree] => traverse(t)
    case t: Option[Tree]      => t ifDefined traverse
  }
}

class ForeachTraverser(f: Tree => Unit) extends TreeTraverser {

  override def traverse(t: Tree) = {
    f(t)
    super.traverse(t)
  }

}
