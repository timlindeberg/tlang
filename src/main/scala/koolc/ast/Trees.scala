package koolc
package ast

import utils._
import analyzer.Symbols._
import analyzer.Types._

object Trees {
  sealed trait Tree extends Positioned

  case class Program(main: MainObject, classes: List[ClassDecl]) extends Tree
  case class MainObject(id: Identifier, stats: List[StatTree]) extends Tree with Symbolic[ClassSymbol]
  case class ClassDecl(var id: TypeIdentifier, var parent: Option[TypeIdentifier], vars: List[VarDecl], methods: List[MethodDecl]) extends Tree with Symbolic[ClassSymbol]
  case class VarDecl(var tpe: TypeTree, var id: Identifier) extends Tree with Symbolic[VariableSymbol]
  case class MethodDecl(var retType: TypeTree, var id: Identifier, var args: List[Formal], var vars: List[VarDecl], stats: List[StatTree], retExpr: ExprTree) extends Tree with Symbolic[MethodSymbol]
  sealed case class Formal(var tpe: TypeTree, id: Identifier) extends Tree with Symbolic[VariableSymbol]

  sealed trait TypeTree extends Tree with Typed {
    def name = this match {
      case TypeIdentifier(value, _) => value
      case IntType()                => "Int"
      case IntArrayType()           => "Int[]"
      case StringType()             => "String"
      case BooleanType()            => "Bool"
    }
  }

  case class IntArrayType() extends TypeTree
  case class IntType() extends TypeTree
  case class BooleanType() extends TypeTree
  case class StringType() extends TypeTree

  sealed trait StatTree extends Tree
  case class Block(stats: List[StatTree]) extends StatTree
  case class If(expr: ExprTree, thn: StatTree, els: Option[StatTree]) extends StatTree
  case class While(expr: ExprTree, stat: StatTree) extends StatTree
  case class Println(expr: ExprTree) extends StatTree
  case class Assign(id: Identifier, expr: ExprTree) extends StatTree
  case class ArrayAssign(id: Identifier, index: ExprTree, expr: ExprTree) extends StatTree

  sealed trait ExprTree extends Tree with Typed
  case class And(lhs: ExprTree, rhs: ExprTree) extends ExprTree
  case class Or(lhs: ExprTree, rhs: ExprTree) extends ExprTree
  case class Plus(lhs: ExprTree, rhs: ExprTree) extends ExprTree
  case class Minus(lhs: ExprTree, rhs: ExprTree) extends ExprTree
  case class Times(lhs: ExprTree, rhs: ExprTree) extends ExprTree
  case class Div(lhs: ExprTree, rhs: ExprTree) extends ExprTree
  case class LessThan(lhs: ExprTree, rhs: ExprTree) extends ExprTree
  case class Equals(lhs: ExprTree, rhs: ExprTree) extends ExprTree
  case class ArrayRead(arr: ExprTree, index: ExprTree) extends ExprTree
  case class ArrayLength(arr: ExprTree) extends ExprTree
  case class MethodCall(obj: ExprTree, meth: Identifier, args: List[ExprTree]) extends ExprTree
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
    override def setType(tpe: Type) = { getSymbol.setType(tpe); this }
  }
  case class TypeIdentifier(var value: String, templateTypes: List[TypeTree] = List()) extends TypeTree with ExprTree with Symbolic[Symbol] {
    // The type of the identifier depends on the type of the symbol
    override def getType: Type = getSymbol match {
      case cs: ClassSymbol    => TObject(cs)
      case vs: VariableSymbol => vs.getType
      case ms: MethodSymbol   => ms.getType
      case es: ErrorSymbol    => TError
    }

    override def setType(tpe: Type) = { getSymbol.setType(tpe); this }
    def isTemplated = !templateTypes.isEmpty
    def templatedClassName(): String = templatedClassName(templateTypes)
    def templatedClassName(templateTypes: List[TypeTree]): String = {
      var tmp = List[String]()
      templateTypes.foreach(x => tmp = x.name :: tmp)
      value + (if (isTemplated) "$" + tmp.reverse.mkString("$") else "")
    }
  }

  case class This() extends ExprTree with Symbolic[ClassSymbol]
  case class NewIntArray(size: ExprTree) extends ExprTree
  case class New(var tpe: TypeIdentifier) extends ExprTree
  case class Not(expr: ExprTree) extends ExprTree

  def traverse(t: Product, f: Product => Unit): Unit = {
    t.productIterator.foreach(_ match {
      case x: List[_] =>
        x.foreach(_ match {
          case x: Product =>
            traverse(x, f)
          case _ =>
        })
      case x: Option[Any] =>
        if (x.isDefined) {
          x match {
            case x: Product =>
              traverse(x, f)
            case _ =>
          }
        }
      case x: Product =>
        traverse(x, f)
      case _ =>
    })
    f(t)
  }
}
