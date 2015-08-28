package tcompiler
package ast

import tcompiler.analyzer.Symbols._
import tcompiler.analyzer.Types._
import tcompiler.utils._

object Trees {

  trait Tree extends Positioned with Product


  case class Program(progPackage: Option[Package], var imports: List[Import], main: Option[MethodDecl], var classes: List[ClassDecl]) extends Tree {
    def getPackageDirectory = progPackage.map(_.identifiers.map(_.value).mkString("/") + "/").getOrElse("")
  }


  trait Import extends Tree {
    val identifiers: List[Identifier]
  }

  case class Package(identifiers: List[Identifier]) extends Tree
  case class RegularImport(identifiers: List[Identifier]) extends Import
  case class WildCardImport(identifiers: List[Identifier]) extends Import
  case class GenericImport(identifiers: List[Identifier]) extends Import

  object ClassDecl {
    def unapply(e: ClassDecl): Option[(ClassIdentifier, Option[ClassIdentifier], List[VarDecl], List[FuncTree])] = e match {
      case InternalClassDecl(id, parent, vars, methods) => Some((id, parent, vars, methods))
      case ExternalClassDecl(id, parent, vars, methods) => Some((id, parent, vars, methods))
      case _                                            => None
    }
  }

  trait ClassDecl extends Tree with Symbolic[ClassSymbol] {
    var id: ClassIdentifier
    var parent: Option[ClassIdentifier]
    val vars: List[VarDecl]
    val methods: List[FuncTree]
  }

  trait Modifiable {
    val modifiers: Set[Modifier]

    def isStatic = modifiers.contains(Static)

    def accessability = modifiers.find(_.isInstanceOf[Accessability]).get.asInstanceOf[Accessability]

  }

  case class InternalClassDecl(var id: ClassIdentifier, var parent: Option[ClassIdentifier], vars: List[VarDecl], methods: List[FuncTree]) extends ClassDecl
  case class ExternalClassDecl(var id: ClassIdentifier, var parent: Option[ClassIdentifier], vars: List[VarDecl], methods: List[FuncTree]) extends ClassDecl
  case class Formal(var tpe: TypeTree, id: Identifier) extends Tree with Symbolic[VariableSymbol]

  trait Modifier extends Tree

  trait Accessability extends Modifier

  case object Public extends Accessability
  case object Private extends Accessability
  case object Protected extends Accessability

  case object Static extends Modifier

  trait FuncTree extends Tree with Symbolic[MethodSymbol] with Modifiable {
    var id: Identifier
    var retType: Option[TypeTree]
    var args: List[Formal]
    val stat: StatTree
    val modifiers: Set[Modifier]

    def signature = id.value + args.map(_.tpe.name).mkString("(", ", ", ")")
  }

  case class MethodDecl(var retType: Option[TypeTree], var id: Identifier, var args: List[Formal], stat: StatTree, modifiers: Set[Modifier]) extends FuncTree
  case class ConstructorDecl(var retType: Option[TypeTree], var id: Identifier, var args: List[Formal], stat: StatTree, modifiers: Set[Modifier]) extends FuncTree
  case class OperatorDecl(var operatorType: ExprTree, var retType: Option[TypeTree], var args: List[Formal], stat: StatTree, modifiers: Set[Modifier], var id: Identifier = new Identifier("")) extends FuncTree

  trait TypeTree extends Tree with Typed {
    def name: String
  }

  case class ArrayType(var tpe: TypeTree) extends TypeTree { def name = tpe.name + "[]"}
  case class IntType() extends TypeTree  { def name = "Int" }
  case class LongType() extends TypeTree { def name = "Long" }
  case class FloatType() extends TypeTree { def name = "Float" }
  case class DoubleType() extends TypeTree { def name = "Double" }
  case class BooleanType() extends TypeTree { def name = "Bool" }
  case class CharType() extends TypeTree { def name = "Char" }
  case class StringType() extends TypeTree { def name = "String" }
  case class UnitType() extends TypeTree { def name = "Unit" }

  trait StatTree extends Tree

  case class VarDecl(var tpe: Option[TypeTree], var id: Identifier, init: Option[ExprTree], modifiers: Set[Modifier]) extends StatTree with Symbolic[VariableSymbol] with Modifiable
  case class Block(stats: List[StatTree]) extends StatTree
  case class If(expr: ExprTree, thn: StatTree, els: Option[StatTree]) extends StatTree
  case class While(expr: ExprTree, stat: StatTree) extends StatTree
  case class For(init: List[StatTree], condition: ExprTree, post: List[StatTree], stat: StatTree) extends StatTree
  case class Print(expr: ExprTree) extends StatTree
  case class Println(expr: ExprTree) extends StatTree
  case class Error(expr: ExprTree) extends StatTree
  case class Return(expr: Option[ExprTree]) extends StatTree

  trait ExprTree extends Tree with Typed

  case class Assign(id: Identifier, expr: ExprTree) extends ExprTree with StatTree
  case class ArrayAssign(arr: ExprTree, index: ExprTree, expr: ExprTree) extends ExprTree with StatTree
  case class FieldRead(obj: ExprTree, id: Identifier) extends ExprTree
  case class FieldAssign(obj: ExprTree, id: Identifier, expr: ExprTree) extends ExprTree with StatTree

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
  case class MethodCall(var obj: ExprTree, meth: Identifier, args: List[ExprTree]) extends ExprTree with StatTree
  case class IntLit(value: Int) extends ExprTree
  case class LongLit(value: Long) extends ExprTree
  case class FloatLit(value: Float) extends ExprTree
  case class DoubleLit(value: Double) extends ExprTree
  case class CharLit(value: Char) extends ExprTree
  case class StringLit(value: String) extends ExprTree
  case class True() extends ExprTree
  case class False() extends ExprTree

  case class Identifier(var value: String) extends ExprTree with Symbolic[Symbol] {
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

  case class ClassIdentifier(var value: String, var templateTypes: List[TypeTree] = List()) extends TypeTree with ExprTree with Symbolic[Symbol] {
    val StartEndSign = "-"
    val Seperator = "$"

    def name = value

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

    def isTemplated = templateTypes.nonEmpty

    def templatedClassName: String = templatedClassName(templateTypes)

    def templatedClassName(templateTypes: List[TypeTree]): String = {
      val tTypes = templateTypes.map {
        case x: ClassIdentifier if x.isTemplated => x.templatedClassName
        case x                                   => x.name
      }
      StartEndSign + value + (if (isTemplated) Seperator + tTypes.mkString(Seperator)) + StartEndSign
    }
  }

  case class This() extends ExprTree with Symbolic[ClassSymbol]
  case class NewArray(var tpe: TypeTree, sizes: List[ExprTree]) extends ExprTree { def dimension = sizes.size }
  case class New(var tpe: ClassIdentifier, args: List[ExprTree]) extends ExprTree
  case class Not(expr: ExprTree) extends ExprTree
  case class Hash(expr: ExprTree) extends ExprTree
  case class Negation(expr: ExprTree) extends ExprTree
  case class LogicNot(expr: ExprTree) extends ExprTree
  case class PreIncrement(id: ExprTree) extends ExprTree with StatTree
  case class PostIncrement(id: ExprTree) extends ExprTree with StatTree
  case class PreDecrement(id: ExprTree) extends ExprTree with StatTree
  case class PostDecrement(id: ExprTree) extends ExprTree with StatTree
  case class Ternary(condition: ExprTree, thn: ExprTree, els: ExprTree) extends ExprTree with StatTree

  case class Empty() extends ExprTree {
    override def toString = ""
  }

  def operatorString(operatorSymbol: OperatorSymbol): String =
    operatorString(operatorSymbol.operatorType, operatorSymbol.argList.map(_.getType))

  def operatorString(exprTree: ExprTree, args: List[Type]): String = {
    exprTree match {
      case _: Plus              => args(0) + " + " + args(1)
      case _: Minus             => args(0) + " - " + args(1)
      case _: Times             => args(0) + " * " + args(1)
      case _: Div               => args(0) + " / " + args(1)
      case _: Modulo            => args(0) + " % " + args(1)
      case _: LogicAnd          => args(0) + " & " + args(1)
      case _: LogicOr           => args(0) + " | " + args(1)
      case _: LogicXor          => args(0) + " ^ " + args(1)
      case _: LeftShift         => args(0) + " << " + args(1)
      case _: RightShift        => args(0) + " >> " + args(1)
      case _: LessThan          => args(0) + " < " + args(1)
      case _: LessThanEquals    => args(0) + " <= " + args(1)
      case _: GreaterThan       => args(0) + " > " + args(1)
      case _: GreaterThanEquals => args(0) + " >= " + args(1)
      case _: Equals            => args(0) + " == " + args(1)
      case _: NotEquals         => args(0) + " != " + args(1)
      case _: LogicNot          => "~" + args(0)
      case _: Not               => "!" + args(0)
      case _: Negation          => "-" + args(0)
      case _: Hash              => "#" + args(0)
      case _: PreIncrement      => "++" + args(0)
      case _: PostIncrement     => args(0) + "++"
      case _: PreDecrement      => "--" + args(0)
      case _: ArrayRead         => "[" + args(0) + "]"
      case _: ArrayAssign       => "[" + args(0) + "]= " + args(1)
    }
  }

  def operatorString(exprTree: ExprTree): String = exprTree match {
    case _: Plus              => "+"
    case _: Minus             => "-"
    case _: Times             => "*"
    case _: Div               => "/"
    case _: Modulo            => "%"
    case _: LogicAnd          => "&"
    case _: LogicOr           => "|"
    case _: LogicXor          => "^"
    case _: LeftShift         => "<<"
    case _: RightShift        => ">>"
    case _: LessThan          => "<"
    case _: LessThanEquals    => "<="
    case _: GreaterThan       => ">"
    case _: GreaterThanEquals => ">="
    case _: Equals            => "=="
    case _: NotEquals         => "!="
    case _: LogicNot          => "~"
    case _: Not               => "!"
    case _: Negation          => "-"
    case _: Hash              => "#"
    case _: PreIncrement      => "++"
    case _: PostIncrement     => "++"
    case _: PreDecrement      => "--"
    case _: ArrayRead         => "[]"
    case _: ArrayAssign       => "[]="
  }

  def isStaticCall(obj: ExprTree) =
    obj match {
      case id@Identifier(name) =>
        id.getSymbol match {
          case _: ClassSymbol => true
          case _              => false
        }
      case _                   => false
    }


  def traverse(t: Product, f: (Product, Product) => Any): Unit = {
    def trav(parent: Product, current: Product, f: (Product, Product) => Any): Unit = {
      current.productIterator.foreach(Some(_) collect {
        case x: List[_]     =>
          x.foreach(Some(_) collect { case x: Product => trav(current, x, f) })
        case x: Option[Any] =>
          x collect { case x: Product => trav(current, x, f) }
        case x: Product     => trav(current, x, f)
      })
      f(parent, current)
    }
    trav(t, t, f)
  }
}
