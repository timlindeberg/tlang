package tcompiler.ast

import tcompiler.ast.Trees._

/**
  * Created by Tim Lindeberg on 5/22/2016.
  */


class TreeTransformer {

  val treeCopy: TreeCopier = new LazyTreeCopier()

  def transform(t: Tree): Tree = {
    t match {
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

      case CompilationUnit(pack, imports, classes, importMap) =>
        treeCopy.CompilationUnit(t, pack, tList(imports), tList(classes), importMap)
      case Package(adress)                                           =>
        treeCopy.Package(t, adress)

      case RegularImport(adress)                                     =>
        treeCopy.RegularImport(t, adress)
      case WildCardImport(adress)                                    =>
        treeCopy.WildCardImport(t, adress)
      case ClassDecl(id, parents, fields, methods, isTrait)               =>
        treeCopy.ClassDecl(t, trans(id), tList(parents), tList(fields), tList(methods), isTrait)
      case MethodDecl(retType, id, args, stat, modifiers)                 =>
        treeCopy.MethodDecl(t, tOption(retType), trans(id), tList(args), tOption(stat), tSet(modifiers))
      case ConstructorDecl(retType, id, args, stat, modifiers)            =>
        treeCopy.ConstructorDecl(t, tOption(retType), trans(id), tList(args), tOption(stat), tSet(modifiers))
      case OperatorDecl(operatorType, retType, args, stat, modifiers) =>
        treeCopy.OperatorDecl(t, operatorType, tOption(retType), tList(args), tOption(stat), tSet(modifiers))

      case Formal(tpe, id)                   =>
        treeCopy.Formal(t, trans(tpe), trans(id))
      case ArrayType(tpe)                    =>
        treeCopy.ArrayType(t, trans(tpe))
      case NullableType(tpe)                 =>
        treeCopy.NullableType(t, trans(tpe))
      case VarDecl(tpe, id, init, modifiers) =>
        treeCopy.VarDecl(t, tOption(tpe), trans(id), tOption(init), tSet(modifiers))
      case Block(stats)                      =>
        treeCopy.Block(t, tList(stats))
      case If(expr, thn, els)                =>
        treeCopy.If(t, trans(expr), trans(thn), tOption(els))
      case While(expr, stat)                 =>
        treeCopy.While(t, trans(expr), trans(stat))
      case For(init, condition, post, stat)  =>
        treeCopy.For(t, tList(init), trans(condition), tList(post), trans(stat))
      case Foreach(varDecl, container, stat) =>
        treeCopy.Foreach(t, trans(varDecl), trans(container), trans(stat))
      case Print(expr)                   =>
        treeCopy.Print(t, trans(expr))
      case Println(expr)                 =>
        treeCopy.Println(t, trans(expr))
      case Error(expr)                   =>
        treeCopy.Error(t, trans(expr))
      case Return(expr)                  =>
        treeCopy.Return(t, tOption(expr))
      case NormalAccess(obj, application) =>
        treeCopy.NormalAccess(t, trans(obj), trans(application))
      case SafeAccess(obj, application) =>
        treeCopy.SafeAccess(t, trans(obj), trans(application))
      case Assign(id, expr)              =>
        treeCopy.Assign(t, trans(id), trans(expr))
      case ArrayAssign(arr, index, expr) =>
        treeCopy.ArrayAssign(t, trans(arr), trans(index), trans(expr))
      case And(lhs, rhs)        =>
        treeCopy.And(t, trans(lhs), trans(rhs))
      case Or(lhs, rhs)         =>
        treeCopy.Or(t, trans(lhs), trans(rhs))
      case LogicAnd(lhs, rhs)   =>
        treeCopy.LogicAnd(t, trans(lhs), trans(rhs))
      case LogicOr(lhs, rhs)    =>
        treeCopy.LogicOr(t, trans(lhs), trans(rhs))
      case LogicXor(lhs, rhs)   =>
        treeCopy.LogicXor(t, trans(lhs), trans(rhs))
      case Plus(lhs, rhs)       =>
        treeCopy.Plus(t, trans(lhs), trans(rhs))
      case Minus(lhs, rhs)      =>
        treeCopy.Minus(t, trans(lhs), trans(rhs))
      case LeftShift(lhs, rhs)  =>
        treeCopy.LeftShift(t, trans(lhs), trans(rhs))
      case RightShift(lhs, rhs) =>
        treeCopy.RightShift(t, trans(lhs), trans(rhs))
      case Times(lhs, rhs)      =>
        treeCopy.Times(t, trans(lhs), trans(rhs))
      case Div(lhs, rhs)        =>
        treeCopy.Div(t, trans(lhs), trans(rhs))
      case Modulo(lhs, rhs)     =>
        treeCopy.Modulo(t, trans(lhs), trans(rhs))

      case LessThan(lhs, rhs)          =>
        treeCopy.LessThan(t, trans(lhs), trans(rhs))
      case LessThanEquals(lhs, rhs)    =>
        treeCopy.LessThanEquals(t, trans(lhs), trans(rhs))
      case GreaterThan(lhs, rhs)       =>
        treeCopy.GreaterThan(t, trans(lhs), trans(rhs))
      case GreaterThanEquals(lhs, rhs) =>
        treeCopy.GreaterThanEquals(t, trans(lhs), trans(rhs))

      case Is(expr, id)          =>
        treeCopy.Is(t, trans(expr), trans(id))
      case As(expr, tpe)         =>
        treeCopy.As(t, trans(expr), trans(tpe))
      case Equals(lhs, rhs)      =>
        treeCopy.Equals(t, trans(lhs), trans(rhs))
      case NotEquals(lhs, rhs)   =>
        treeCopy.NotEquals(t, trans(lhs), trans(rhs))
      case ArrayRead(arr, index) =>
        treeCopy.ArrayRead(t, trans(arr), trans(index))
      case ArraySlice(arr, start, end)           =>
        treeCopy.ArraySlice(t, trans(arr), tOption(start), tOption(end))
      case MethodCall(meth, args)           =>
        treeCopy.MethodCall(t, trans(meth), tList(args))
      case IntLit(value)                         =>
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
        treeCopy.ArrayLit(t, tList(expressions))
      case ClassID(value, templateTypes) =>
        treeCopy.ClassID(t, value, tList(templateTypes))
      case VariableID(value)             =>
        treeCopy.VariableID(t, value)
      case MethodID(value)               =>
        treeCopy.MethodID(t, value)
      case Super(specifier)              =>
        treeCopy.Super(t, tOption(specifier))
      case NewArray(tpe, sizes)          =>
        treeCopy.NewArray(t, trans(tpe), tList(sizes))
      case New(tpe, args)                =>
        treeCopy.New(t, trans(tpe), tList(args))
      case Not(expr)                     =>
        treeCopy.Not(t, trans(expr))
      case Hash(expr)                    =>
        treeCopy.Hash(t, trans(expr))
      case Negation(expr)                        =>
        treeCopy.Negation(t, trans(expr))
      case LogicNot(expr)                        =>
        treeCopy.LogicNot(t, trans(expr))
      case PreIncrement(expr)                    =>
        treeCopy.PreIncrement(t, trans(expr))
      case PostIncrement(expr)                   =>
        treeCopy.PostIncrement(t, trans(expr))
      case PreDecrement(expr)                    =>
        treeCopy.PreDecrement(t, trans(expr))
      case PostDecrement(expr)                   =>
        treeCopy.PostDecrement(t, trans(expr))
      case Ternary(condition, thn, els)          =>
        treeCopy.Ternary(t, trans(condition), trans(thn), trans(els))
      case Elvis(nullableValue, ifNull)          =>
        treeCopy.Elvis(t, trans(nullableValue), trans(ifNull))
      case GeneratedExpr(block) =>
        treeCopy.GeneratedExpr(t, block)
      case IfDup(expr) =>
        treeCopy.IfDup(t, expr)
    }
  }

  protected def trans[T <: Tree](tree: T) = transform(tree).asInstanceOf[T]
  protected def tList[T <: Tree](list: List[T]) = list.map(transform).asInstanceOf[List[T]]
  protected def tSet[T <: Tree](set: Set[T]) = set.map(transform).asInstanceOf[Set[T]]
  protected def tOption[T <: Tree](op: Option[T]) = op match {
    case Some(p) => Some(transform(p).asInstanceOf[T])
    case None    => None
  }
}
