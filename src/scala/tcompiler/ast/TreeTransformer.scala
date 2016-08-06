package tcompiler.ast

import tcompiler.ast.Trees._

/**
  * Created by Tim Lindeberg on 5/22/2016.
  */


class TreeTransformer {

  val treeCopy: TreeCopier = new LazyTreeCopier()

  def transform(t: Tree): Tree = t match {
    case Public()      => treeCopy.Public(t)
    case Private()     => treeCopy.Private(t)
    case Protected()   => treeCopy.Protected(t)
    case Static()      => treeCopy.Static(t)
    case Implicit()    => treeCopy.Implicit(t)
    case Final()       => treeCopy.Final(t)
    case IntType()     => treeCopy.IntType(t)
    case LongType()    => treeCopy.LongType(t)
    case FloatType()   => treeCopy.FloatType(t)
    case DoubleType()  => treeCopy.DoubleType(t)
    case BooleanType() => treeCopy.BooleanType(t)
    case CharType()    => treeCopy.CharType(t)
    case UnitType()    => treeCopy.UnitType(t)
    case Break()       => treeCopy.Break(t)
    case Continue()    => treeCopy.Continue(t)
    case TrueLit()     => treeCopy.TrueLit(t)
    case FalseLit()    => treeCopy.FalseLit(t)
    case NullLit()     => treeCopy.NullLit(t)
    case This()        => treeCopy.This(t)
    case Empty()       => treeCopy.Empty(t)

    case CompilationUnit(pack, classes, importMap) =>
      treeCopy.CompilationUnit(t, pack, tr(classes), importMap)
    case Package(adress)                                    =>
      treeCopy.Package(t, adress)

    case RegularImport(adress)                                      =>
      treeCopy.RegularImport(t, adress)
    case WildCardImport(adress)                                     =>
      treeCopy.WildCardImport(t, adress)
    case ClassDecl(id, parents, fields, methods, isTrait)           =>
      treeCopy.ClassDecl(t, tr(id), tr(parents), tr(fields), tr(methods), isTrait)
    case MethodDecl(retType, id, args, stat, modifiers)             =>
      treeCopy.MethodDecl(t, tr(retType), tr(id), tr(args), tr(stat), tr(modifiers))
    case ConstructorDecl(retType, id, args, stat, modifiers)        =>
      treeCopy.ConstructorDecl(t, tr(retType), tr(id), tr(args), tr(stat), tr(modifiers))
    case OperatorDecl(operatorType, retType, args, stat, modifiers) =>
      treeCopy.OperatorDecl(t, operatorType, tr(retType), tr(args), tr(stat), tr(modifiers))

    case Formal(tpe, id)                   =>
      treeCopy.Formal(t, tr(tpe), tr(id))
    case ArrayType(tpe)                    =>
      treeCopy.ArrayType(t, tr(tpe))
    case NullableType(tpe)                 =>
      treeCopy.NullableType(t, tr(tpe))
    case VarDecl(tpe, id, init, modifiers) =>
      treeCopy.VarDecl(t, tr(tpe), tr(id), tr(init), tr(modifiers))
    case Block(stats)                      =>
      treeCopy.Block(t, tr(stats))
    case If(condition, thn, els)                =>
      treeCopy.If(t, tr(condition), tr(thn), tr(els))
    case While(condition, stat)                 =>
      treeCopy.While(t, tr(condition), tr(stat))
    case For(init, condition, post, stat)  =>
      treeCopy.For(t, tr(init), tr(condition), tr(post), tr(stat))
    case Foreach(varDecl, container, stat) =>
      treeCopy.Foreach(t, tr(varDecl), tr(container), tr(stat))
    case Print(expr)                       =>
      treeCopy.Print(t, tr(expr))
    case Println(expr)                     =>
      treeCopy.Println(t, tr(expr))
    case Error(expr)                       =>
      treeCopy.Error(t, tr(expr))
    case Return(expr)                      =>
      treeCopy.Return(t, tr(expr))
    case NormalAccess(obj, application)    =>
      treeCopy.NormalAccess(t, tr(obj), tr(application))
    case SafeAccess(obj, application)      =>
      treeCopy.SafeAccess(t, tr(obj), tr(application))
    case Assign(id, expr)                  =>
      treeCopy.Assign(t, tr(id), tr(expr))
    case ArrayAssign(arr, index, expr)     =>
      treeCopy.ArrayAssign(t, tr(arr), tr(index), tr(expr))
    case And(lhs, rhs)                     =>
      treeCopy.And(t, tr(lhs), tr(rhs))
    case Or(lhs, rhs)                      =>
      treeCopy.Or(t, tr(lhs), tr(rhs))
    case LogicAnd(lhs, rhs)                =>
      treeCopy.LogicAnd(t, tr(lhs), tr(rhs))
    case LogicOr(lhs, rhs)                 =>
      treeCopy.LogicOr(t, tr(lhs), tr(rhs))
    case LogicXor(lhs, rhs)                =>
      treeCopy.LogicXor(t, tr(lhs), tr(rhs))
    case Plus(lhs, rhs)                    =>
      treeCopy.Plus(t, tr(lhs), tr(rhs))
    case Minus(lhs, rhs)                   =>
      treeCopy.Minus(t, tr(lhs), tr(rhs))
    case LeftShift(lhs, rhs)               =>
      treeCopy.LeftShift(t, tr(lhs), tr(rhs))
    case RightShift(lhs, rhs)              =>
      treeCopy.RightShift(t, tr(lhs), tr(rhs))
    case Times(lhs, rhs)                   =>
      treeCopy.Times(t, tr(lhs), tr(rhs))
    case Div(lhs, rhs)                     =>
      treeCopy.Div(t, tr(lhs), tr(rhs))
    case Modulo(lhs, rhs)                  =>
      treeCopy.Modulo(t, tr(lhs), tr(rhs))

    case LessThan(lhs, rhs)          =>
      treeCopy.LessThan(t, tr(lhs), tr(rhs))
    case LessThanEquals(lhs, rhs)    =>
      treeCopy.LessThanEquals(t, tr(lhs), tr(rhs))
    case GreaterThan(lhs, rhs)       =>
      treeCopy.GreaterThan(t, tr(lhs), tr(rhs))
    case GreaterThanEquals(lhs, rhs) =>
      treeCopy.GreaterThanEquals(t, tr(lhs), tr(rhs))

    case Is(expr, tpe)                  =>
      treeCopy.Is(t, tr(expr), tr(tpe))
    case As(expr, tpe)                 =>
      treeCopy.As(t, tr(expr), tr(tpe))
    case Equals(lhs, rhs)              =>
      treeCopy.Equals(t, tr(lhs), tr(rhs))
    case NotEquals(lhs, rhs)           =>
      treeCopy.NotEquals(t, tr(lhs), tr(rhs))
    case ArrayRead(arr, index)         =>
      treeCopy.ArrayRead(t, tr(arr), tr(index))
    case ArraySlice(arr, start, end)   =>
      treeCopy.ArraySlice(t, tr(arr), tr(start), tr(end))
    case MethodCall(meth, args)        =>
      treeCopy.MethodCall(t, tr(meth), tr(args))
    case IntLit(value)                 =>
      treeCopy.IntLit(t, value)
    case LongLit(value)                =>
      treeCopy.LongLit(t, value)
    case FloatLit(value)               =>
      treeCopy.FloatLit(t, value)
    case DoubleLit(value)              =>
      treeCopy.DoubleLit(t, value)
    case CharLit(value)                =>
      treeCopy.CharLit(t, value)
    case StringLit(value)              =>
      treeCopy.StringLit(t, value)
    case ArrayLit(expressions)         =>
      treeCopy.ArrayLit(t, tr(expressions))
    case ClassID(value, templateTypes) =>
      treeCopy.ClassID(t, value, tr(templateTypes))
    case VariableID(value)             =>
      treeCopy.VariableID(t, value)
    case MethodID(value)               =>
      treeCopy.MethodID(t, value)
    case Super(specifier)              =>
      treeCopy.Super(t, tr(specifier))
    case NewArray(tpe, sizes)          =>
      treeCopy.NewArray(t, tr(tpe), tr(sizes))
    case New(tpe, args)                =>
      treeCopy.New(t, tr(tpe), tr(args))
    case Not(expr)                     =>
      treeCopy.Not(t, tr(expr))
    case Hash(expr)                    =>
      treeCopy.Hash(t, tr(expr))
    case Negation(expr)                =>
      treeCopy.Negation(t, tr(expr))
    case LogicNot(expr)                =>
      treeCopy.LogicNot(t, tr(expr))
    case PreIncrement(expr)            =>
      treeCopy.PreIncrement(t, tr(expr))
    case PostIncrement(expr)          =>
      treeCopy.PostIncrement(t, tr(expr))
    case PreDecrement(expr)           =>
      treeCopy.PreDecrement(t, tr(expr))
    case PostDecrement(expr)          =>
      treeCopy.PostDecrement(t, tr(expr))
    case Ternary(condition, thn, els) =>
      treeCopy.Ternary(t, tr(condition), tr(thn), tr(els))
    case Elvis(nullableValue, ifNull) =>
      treeCopy.Elvis(t, tr(nullableValue), tr(ifNull))
    case ExtractNullable(expr) =>
      treeCopy.ExtractNullable(t, tr(expr))
    case GeneratedExpr(block)         =>
      treeCopy.GeneratedExpr(t, block)
    case PutValue(expr)               =>
      treeCopy.IfDup(t, expr)
  }

  protected def tr[T <: Tree](tree: T) = transform(tree).asInstanceOf[T]
  protected def tr[T <: Tree](list: List[T]) = list.map(transform).asInstanceOf[List[T]]
  protected def tr[T <: Tree](set: Set[T]) = set.map(transform).asInstanceOf[Set[T]]
  protected def tr[T <: Tree](op: Option[T]) = op.map(transform).asInstanceOf[Option[T]]
}
