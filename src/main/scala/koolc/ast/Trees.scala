package koolc
package ast

import koolc.lexer.Tokens._
import utils._
import analyzer.Symbols._
import analyzer.Types._

import scala.annotation.tailrec

object Trees {
  trait Tree extends Positioned with Product


  case class Program(main: MainObject, classes: List[ClassDecl]) extends Tree
  case class MainObject(id: Identifier, stats: List[StatTree]) extends Tree with Symbolic[ClassSymbol]
  case class ClassDecl(var id: TypeIdentifier, var parent: Option[TypeIdentifier], vars: List[VarDecl], methods: List[FuncTree]) extends Tree with Symbolic[ClassSymbol]
  case class VarDecl(var tpe: TypeTree, var id: Identifier) extends Tree with Symbolic[VariableSymbol]
  case class Formal(var tpe: TypeTree, id: Identifier) extends Tree with Symbolic  [VariableSymbol]

  trait Accessability
  case object Public extends  Accessability
  case object Private extends  Accessability
  case object Protected extends  Accessability

  trait FuncTree extends Tree with Symbolic[MethodSymbol]{
    var id: Identifier
    var args: List[Formal]
    var vars: List[VarDecl]
    val stats: List[StatTree]
    val access: Accessability

    def signature = id.value + args.map(_.tpe.name).mkString("(", ", ", ")")
  }

  case class MethodDecl(var retType: TypeTree, var id: Identifier, var args: List[Formal], var vars: List[VarDecl], stats: List[StatTree], access: Accessability) extends FuncTree
  case class ConstructorDecl(var id: Identifier, var args: List[Formal], var vars: List[VarDecl], stats: List[StatTree], access: Accessability) extends FuncTree

  trait TypeTree extends Tree with Typed {
    def name: String = this match {
      case TypeIdentifier(value, _) => value
      case ArrayType(tpe)           => tpe.name + "[]"
      case IntType()                => "Int"
      case StringType()             => "String"
      case BooleanType()            => "Bool"
      case UnitType()               => "Unit"
    }
  }

  case class ArrayType(tpe: TypeTree) extends TypeTree
  case class IntType() extends TypeTree
  case class BooleanType() extends TypeTree
  case class StringType() extends TypeTree
  case class UnitType() extends TypeTree

  trait StatTree extends Tree
  case class Block(stats: List[StatTree]) extends StatTree
  case class If(expr: ExprTree, thn: StatTree, els: Option[StatTree]) extends StatTree
  case class While(expr: ExprTree, stat: StatTree) extends StatTree
  case class For(init: List[Assign], condition: ExprTree, post: List[StatTree], stat: StatTree) extends StatTree
  case class Print(expr: ExprTree) extends StatTree
  case class Println(expr: ExprTree) extends StatTree
  case class Assign(id: Identifier, expr: ExprTree) extends StatTree
  case class PlusAssign(id: Identifier, expr: ExprTree) extends StatTree
  case class MinusAssign(id: Identifier, expr: ExprTree) extends StatTree
  case class MulAssign(id: Identifier, expr: ExprTree) extends StatTree
  case class DivAssign(id: Identifier, expr: ExprTree) extends StatTree
  case class ModAssign(id: Identifier, expr: ExprTree) extends StatTree
  case class AndAssign(id: Identifier, expr: ExprTree) extends StatTree
  case class OrAssign(id: Identifier, expr: ExprTree) extends StatTree
  case class XorAssign(id: Identifier, expr: ExprTree) extends StatTree
  case class LeftShiftAssign(id: Identifier, expr: ExprTree) extends StatTree
  case class RightShiftAssign(id: Identifier, expr: ExprTree) extends StatTree
  case class ArrayAssign(id: Identifier, index: ExprTree, expr: ExprTree) extends StatTree
  case class Return(expr: Option[ExprTree]) extends StatTree

  trait ExprTree extends Tree with Typed
  case class And(lhs: ExprTree, rhs: ExprTree) extends ExprTree
  case class Or(lhs: ExprTree, rhs: ExprTree) extends ExprTree
  case class LogicAnd(lhs: ExprTree, rhs: ExprTree) extends ExprTree
  case class LogicOr(lhs: ExprTree, rhs: ExprTree) extends ExprTree
  case class LogicXor(lhs: ExprTree, rhs: ExprTree) extends ExprTree
  case class Plus(lhs: ExprTree, rhs: ExprTree) extends ExprTree
  case class Minus(lhs: ExprTree, rhs: ExprTree) extends ExprTree
  case class LeftShift(lhs: ExprTree, rhs: ExprTree) extends ExprTree
  case class RightShift(lhs: ExprTree, rhs: ExprTree) extends ExprTree
  case class Times(lhs: ExprTree, rhs: ExprTree) extends ExprTree
  case class Div(lhs: ExprTree, rhs: ExprTree) extends ExprTree
  case class Modulo(lhs: ExprTree, rhs: ExprTree) extends ExprTree
  case class LessThan(lhs: ExprTree, rhs: ExprTree) extends ExprTree
  case class LessThanEquals(lhs: ExprTree, rhs: ExprTree) extends ExprTree
  case class GreaterThan(lhs: ExprTree, rhs: ExprTree) extends ExprTree
  case class GreaterThanEquals(lhs: ExprTree, rhs: ExprTree) extends ExprTree
  case class Instance(expr: ExprTree, id: Identifier) extends ExprTree
  case class As(expr: ExprTree, tpe: TypeTree) extends ExprTree
  case class Equals(lhs: ExprTree, rhs: ExprTree) extends ExprTree
  case class NotEquals(lhs: ExprTree, rhs: ExprTree) extends ExprTree
  case class ArrayRead(arr: ExprTree, index: ExprTree) extends ExprTree
  case class ArrayLength(arr: ExprTree) extends ExprTree
  case class MethodCall(obj: ExprTree, meth: Identifier, args: List[ExprTree]) extends ExprTree with StatTree
  case class IntLit(value: Int) extends ExprTree
  case class StringLit(value: String) extends ExprTree

  case class True() extends ExprTree
  case class False() extends ExprTree

  case class Identifier(value: String) extends ExprTree with Symbolic[Symbol] {
    // The type of the identifier depends on the type of the symbol
    override def getType: Type = getSymbol match {
      case cs: ClassSymbol    => TObject(cs)
      case vs: VariableSymbol => vs.getType
      case ms: MethodSymbol   => ms.getType
      case es: ErrorSymbol    => TError
    }
    override def setType(tpe: Type) = {
      getSymbol.setType(tpe)
      this
    }
  }
  case class TypeIdentifier(var value: String, var templateTypes: List[TypeTree] = List()) extends TypeTree with ExprTree with Symbolic[Symbol] {
    val StartEndSign = "-"
    val Seperator = "$"
    
    // The type of the identifier depends on the type of the symbol
    override def getType: Type = getSymbol match {
      case cs: ClassSymbol    => TObject(cs)
      case vs: VariableSymbol => vs.getType
      case ms: MethodSymbol   => ms.getType
      case es: ErrorSymbol    => TError
    }

    override def setType(tpe: Type) = { getSymbol.setType(tpe); this }
    def isTemplated = templateTypes.nonEmpty
    def templatedClassName(): String = templatedClassName(templateTypes)
    def templatedClassName(templateTypes: List[TypeTree]): String = {
      val tTypes = templateTypes.map(_ match {
        case x: TypeIdentifier if x.isTemplated => x.templatedClassName
        case x                                  => x.name
      })
      StartEndSign + value + (if (isTemplated) Seperator + tTypes.mkString(Seperator)) + StartEndSign
    }
  }

  case class This() extends ExprTree with Symbolic[ClassSymbol]
  case class NewArray(tpe: TypeTree, size: ExprTree) extends ExprTree
  case class New(var tpe: TypeIdentifier, args: List[ExprTree]) extends ExprTree
  case class Not(expr: ExprTree) extends ExprTree
  case class Negation(expr: ExprTree) extends ExprTree
  case class LogicNot(expr: ExprTree) extends ExprTree
  case class PreIncrement(id: Identifier) extends ExprTree with StatTree
  case class PostIncrement(id: Identifier) extends ExprTree with StatTree
  case class PreDecrement(id: Identifier) extends ExprTree with StatTree
  case class PostDecrement(id: Identifier) extends ExprTree with StatTree
  case class Ternary(condition: ExprTree, thn: ExprTree, els: ExprTree) extends ExprTree with StatTree

  // Collections for easier pattern matching

  object PrintStatement {
    def unapply(e: StatTree): Option[ExprTree] = e match {
      case Print(expr)   => Some(expr)
      case Println(expr) => Some(expr)
      case _             => None
    }
  }

  object Assignment {
    def unapply(e: StatTree): Option[(Identifier, ExprTree)] = e match {
      case Assign(id, expr)           => Some((id, expr))
      case PlusAssign(id, expr)       => Some((id, expr))
      case MinusAssign(id, expr)      => Some((id, expr))
      case MulAssign(id, expr)        => Some((id, expr))
      case DivAssign(id, expr)        => Some((id, expr))
      case ModAssign(id, expr)        => Some((id, expr))
      case AndAssign(id, expr)        => Some((id, expr))
      case OrAssign(id, expr)         => Some((id, expr))
      case XorAssign(id, expr)        => Some((id, expr))
      case LeftShiftAssign(id, expr)  => Some((id, expr))
      case RightShiftAssign(id, expr) => Some((id, expr))
      case _                          => None
    }
  }

  object MathBinaryExpr {
    def unapply(e: ExprTree): Option[(ExprTree, ExprTree)] = e match {
      case Plus(lhs, rhs)       => Some((lhs, rhs))
      case Minus(lhs, rhs)      => Some((lhs, rhs))
      case LogicAnd(lhs, rhs)   => Some((lhs, rhs))
      case LogicOr(lhs, rhs)    => Some((lhs, rhs))
      case LogicXor(lhs, rhs)   => Some((lhs, rhs))
      case Times(lhs, rhs)      => Some((lhs, rhs))
      case Div(lhs, rhs)        => Some((lhs, rhs))
      case Modulo(lhs, rhs)     => Some((lhs, rhs))
      case LeftShift(lhs, rhs)  => Some((lhs, rhs))
      case RightShift(lhs, rhs) => Some((lhs, rhs))
      case _                    => None
    }
  }

  object Comparison {
    def unapply(e: ExprTree): Option[(ExprTree, ExprTree)] = e match {
      case And(lhs, rhs)               => Some((lhs, rhs))
      case Or(lhs, rhs)                => Some((lhs, rhs))
      case LessThan(lhs, rhs)          => Some((lhs, rhs))
      case LessThanEquals(lhs, rhs)    => Some((lhs, rhs))
      case GreaterThan(lhs, rhs)       => Some((lhs, rhs))
      case GreaterThanEquals(lhs, rhs) => Some((lhs, rhs))
      case Equals(lhs, rhs)            => Some((lhs, rhs))
      case NotEquals(lhs, rhs)         => Some((lhs, rhs))
      case Instance(lhs, rhs)        => Some((lhs, rhs))
      case _                           => None
    }
  }

  object IncrementDecrement {
    def unapply(e: Tree): Option[ExprTree] = e match {
      case PreIncrement(id)  => Some(id)
      case PostIncrement(id) => Some(id)
      case PreDecrement(id)  => Some(id)
      case PostDecrement(id) => Some(id)
      case _                 => None
    }
  }

  def traverse(t: Product, f: (Product, Product) => Any): Unit = {
    def trav(parent: Product, current: Product, f: (Product, Product) => Any): Unit = {
      current.productIterator.foreach(Some(_) collect {
        case x: List[_] =>
          x.foreach(Some(_) collect { case x: Product => trav(current, x, f) })
        case x: Option[Any] =>
          x collect { case x: Product => trav(current, x, f) }
        case x: Product => trav(current, x, f)
      })
      f(parent, current)
    }
    trav(t, t, f)
  }
}
