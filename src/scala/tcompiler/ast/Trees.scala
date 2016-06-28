package tcompiler
package ast

import tcompiler.analyzer.Symbols._
import tcompiler.analyzer.Types._
import tcompiler.utils._
import scala.collection.mutable

object Trees {

  trait Tree extends Positioned with Product {

    def foreach(f: Tree => Unit) = {
      val traverser = new TreeTraverser {
        override def traverse(t: Tree) = {
          f(t)
          super.traverse(t)
        }
      }
      traverser.traverse(this)
    }

    def find(p: Tree => Boolean): Option[Tree] = {
      val traverser = new TreeTraverser {
        var result: Option[Tree] = None

        override def traverse(t: Tree) =
          if (p(t)) result = Some(t)
          else super.traverse(t)
      }
      traverser.traverse(this)
      traverser.result
    }

    def forAll(p: Tree => Boolean): Boolean = {
      val traverser = new TreeTraverser {
        var result = true

        override def traverse(t: Tree) =
          if (!p(t)) result = false
          else super.traverse(t)
      }
      traverser.traverse(this)
      traverser.result
    }

    def children: List[Tree] = {
      def subtrees(x: Any): List[Tree] = x match {
        case Empty()     => Nil
        case t: Tree     => List(t)
        case xs: List[_] => xs flatMap subtrees
        case _           => Nil
      }
      productIterator.toList flatMap subtrees
    }

    def exists(p: Tree => Boolean): Boolean = find(p).isDefined

    def copyTree(): this.type = {
      val copier = new TreeTransformer {
        override val treeCopy = new StrictTreeCopier()
      }
      copier.transform(this).asInstanceOf[this.type]
    }

    def copyAttrs(t: Tree): this.type = {
      setPos(t)
      copySymbolTrees(this, t)

      this match {
        case typed: Typed if t.isInstanceOf[Typed] =>
          val tpe = t.asInstanceOf[Typed]
          typed.setType(tpe.getType)
        case _                                     =>
      }
      this
    }

    private def copySymbolTrees[T <: Symbol, U <: Symbol](to: Tree, from: Tree): Unit = {
      if (to.getClass != from.getClass)
        return

      if (!from.isInstanceOf[Symbolic[_]] || !from.asInstanceOf[Symbolic[_]].hasSymbol)
        return

      // This is not very elegant but is one way to get around type erasure
      from match {
        case x: ClassDecl  => to.asInstanceOf[ClassDecl].setSymbol(x.getSymbol)
        case x: Identifier => to.asInstanceOf[Identifier].setSymbol(x.getSymbol)
        case x: FuncTree   => to.asInstanceOf[FuncTree].setSymbol(x.getSymbol)
        case x: Formal     => to.asInstanceOf[Formal].setSymbol(x.getSymbol)
        case x: VarDecl    => to.asInstanceOf[VarDecl].setSymbol(x.getSymbol)
        case x: This       => to.asInstanceOf[This].setSymbol(x.getSymbol)
        case x: Super      => to.asInstanceOf[Super].setSymbol(x.getSymbol)
      }
    }
  }


  // Signals that the node is a leaf and no further recursion is necessary
  trait LeafTree

  /*-------------------------------- Top level Trees --------------------------------*/

  case class Program(
    pack: Option[Package],
    var imports: List[Import],
    var classes: List[ClassDecl],
    importMap: mutable.Map[String, String]) extends Tree {
    def getPackageDirectory = pack.map(_.identifiers.map(_.value).mkString("/")).getOrElse("")
    def getPackageName(name: String) = pack match {
      case Some(pack) => (pack.identifiers.map(_.value) :+ name).mkString(".")
      case None       => name
    }

    def importNames = imports map importName

    def importName(imp: Import): String = {
      val importName = imp.identifiers.map(_.value).mkString(".")
      getFullName(importName)
    }

    def importName(typeId: ClassIdentifier): String = {
      val name = typeId.value.replaceAll("::", ".")
      getFullName(name)
    }

    def getFullName(name: String) = {
      importMap.getOrElse(name, name).replaceAll("::", ".")
    }
  }

  /*-------------------------------- Package and Import Trees --------------------------------*/

  case class Package(identifiers: List[Identifier]) extends Tree

  object Import {
    def unapply(i: Import): Option[List[Identifier]] = i match {
      case i: Import => Some(i.identifiers)
      case _         => None
    }
  }

  trait Import extends Tree {
    val identifiers: List[Identifier]
  }

  case class RegularImport(identifiers: List[Identifier]) extends Import
  case class WildCardImport(identifiers: List[Identifier]) extends Import


  /*-------------------------------- Class Declaration Trees --------------------------------*/

  case class ClassDecl(
    var id: ClassIdentifier,
    var parents: List[ClassIdentifier],
    var fields: List[VarDecl],
    var methods: List[FuncTree],
    var isAbstract: Boolean) extends Tree with Symbolic[ClassSymbol] {
    def implementedTraits = parents.filter(_.getSymbol.isAbstract)
  }


  /*-------------------------------- Modifier Trees --------------------------------*/

  trait Modifier extends Tree with LeafTree

  trait Accessability extends Modifier

  case class Public() extends Accessability
  case class Private() extends Accessability
  case class Protected() extends Accessability

  case class Static() extends Modifier
  case class Implicit() extends Modifier
  case class Final() extends Modifier

  trait Modifiable {
    val modifiers: Set[Modifier]
    def isStatic = modifiers.contains(Static())
    def accessability = modifiers.find(_.isInstanceOf[Accessability]).get.asInstanceOf[Accessability]
  }

  /*-------------------------------- Function Declaration Trees --------------------------------*/


  object FuncTree {
    def unapply(f: FuncTree): Option[(Identifier, Option[TypeTree], List[Formal], Option[StatTree], Set[Modifier])] = f match {
      case f: FuncTree => Some(f.id, f.retType, f.args, f.stat, f.modifiers)
      case _           => None
    }
  }

  trait FuncTree extends Tree with Symbolic[MethodSymbol] with Modifiable {
    var id       : Identifier
    var retType  : Option[TypeTree]
    var args     : List[Formal]
    val stat     : Option[StatTree]
    val modifiers: Set[Modifier]

    def isMain = this match {
      case MethodDecl(Some(UnitType()), Identifier("main"), Formal(ArrayType(StringType()), _) :: Nil, _, _) =>
        modifiers.size == 2 &&
          modifiers.contains(Public()) &&
          modifiers.contains(Static()) &&
          args.head.id.value == "args"
      case _                                                                                                 => false
    }

    def isAbstract = stat.isEmpty

    def signature = id.value + args.map(_.tpe.name).mkString("(", ", ", ")")

  }

  case class MethodDecl(var retType: Option[TypeTree],
    var id: Identifier,
    var args: List[Formal],
    stat: Option[StatTree],
    modifiers: Set[Modifier]) extends FuncTree
  case class ConstructorDecl(var retType: Option[TypeTree],
    var id: Identifier,
    var args: List[Formal],
    stat: Option[StatTree],
    modifiers: Set[Modifier]) extends FuncTree
  case class OperatorDecl(var operatorType: OperatorTree,
    var retType: Option[TypeTree],
    var args: List[Formal],
    stat: Option[StatTree],
    modifiers: Set[Modifier],
    var id: Identifier = new Identifier("")) extends FuncTree

  case class Formal(var tpe: TypeTree, id: Identifier) extends Tree with Symbolic[VariableSymbol]

  /*-------------------------------- Type Trees --------------------------------*/

  trait TypeTree extends Tree with Typed {
    val name: String
  }

  case class ArrayType(var tpe: TypeTree) extends TypeTree {val name = tpe.name + "[]"}
  case class NullableType(var tpe: TypeTree) extends TypeTree {val name = tpe.name + "?"}
  case class IntType() extends TypeTree with LeafTree {val name = "Int"}
  case class LongType() extends TypeTree with LeafTree {val name = "Long"}
  case class FloatType() extends TypeTree with LeafTree {val name = "Float"}
  case class DoubleType() extends TypeTree with LeafTree {val name = "Double"}
  case class BooleanType() extends TypeTree with LeafTree {val name = "Bool"}
  case class CharType() extends TypeTree with LeafTree {val name = "Char"}
  case class StringType() extends TypeTree with LeafTree {val name = "String"}
  case class UnitType() extends TypeTree with LeafTree {val name = "Unit"}

  /*-------------------------------- Statement Trees --------------------------------*/

  trait StatTree extends Tree

  trait PrintStatTree extends StatTree {
    val expr: ExprTree
  }
  object PrintStatTree {
    def unapply(e: PrintStatTree): Option[ExprTree] = e match {
      case e: PrintStatTree => Some(e.expr)
      case _                => None
    }
  }


  case class VarDecl(var tpe: Option[TypeTree], var id: Identifier, init: Option[ExprTree], modifiers: Set[Modifier]) extends StatTree with Symbolic[VariableSymbol] with Modifiable
  case class Block(stats: List[StatTree]) extends StatTree
  case class If(expr: ExprTree, thn: StatTree, els: Option[StatTree]) extends StatTree
  case class While(expr: ExprTree, stat: StatTree) extends StatTree
  case class For(init: List[StatTree], condition: ExprTree, post: List[StatTree], stat: StatTree) extends StatTree
  case class Foreach(varDecl: VarDecl, container: ExprTree, stat: StatTree) extends StatTree

  case class Error(expr: ExprTree) extends StatTree
  case class Return(expr: Option[ExprTree]) extends StatTree
  case class Break() extends StatTree with LeafTree
  case class Continue() extends StatTree with LeafTree

  case class Print(expr: ExprTree) extends PrintStatTree
  case class Println(expr: ExprTree) extends PrintStatTree

  /*-------------------------------- Binary Operator Trees --------------------------------*/

  trait ExprTree extends StatTree with Typed


  trait OperatorTree extends ExprTree {
    val op: String
    def operatorString(args: List[Any]): String
  }

  trait BinaryOperatorTree extends OperatorTree {
    val lhs: ExprTree
    val rhs: ExprTree

    def operatorString(args: List[Any]): String = s"${args(0)} $op ${args(1)}"
  }

  object BinaryOperatorTree {
    def unapply(e: BinaryOperatorTree): Option[(ExprTree, ExprTree)] = e match {
      case e: BinaryOperatorTree => Some(e.lhs, e.rhs)
      case _                     => None
    }
  }

  trait ArithmeticOperatorTree extends BinaryOperatorTree
  object ArithmeticOperatorTree {
    def unapply(e: ArithmeticOperatorTree): Option[(ExprTree, ExprTree)] = e match {
      case e: ArithmeticOperatorTree => Some(e.lhs, e.rhs)
      case _                         => None
    }
  }

  trait ShiftOperatorTree extends BinaryOperatorTree
  object ShiftOperatorTree {
    def unapply(e: ShiftOperatorTree): Option[(ExprTree, ExprTree)] = e match {
      case e: ShiftOperatorTree => Some(e.lhs, e.rhs)
      case _                    => None
    }
  }

  trait LogicalOperatorTree extends BinaryOperatorTree
  object LogicalOperatorTree {
    def unapply(e: LogicalOperatorTree): Option[(ExprTree, ExprTree)] = e match {
      case e: LogicalOperatorTree => Some(e.lhs, e.rhs)
      case _                      => None
    }
  }

  trait ComparisonOperatorTree extends BinaryOperatorTree
  object ComparisonOperatorTree {
    def unapply(e: ComparisonOperatorTree): Option[(ExprTree, ExprTree)] = e match {
      case e: ComparisonOperatorTree => Some(e.lhs, e.rhs)
      case _                         => None
    }
  }

  trait EqualsOperatorTree extends BinaryOperatorTree
  object EqualsOperatorTree {
    def unapply(e: EqualsOperatorTree): Option[(ExprTree, ExprTree)] = e match {
      case e: EqualsOperatorTree => Some(e.lhs, e.rhs)
      case _                     => None
    }
  }

  case class Plus(lhs: ExprTree, rhs: ExprTree) extends ArithmeticOperatorTree {val op = "+"}
  case class Minus(lhs: ExprTree, rhs: ExprTree) extends ArithmeticOperatorTree {val op = "-"}
  case class Times(lhs: ExprTree, rhs: ExprTree) extends ArithmeticOperatorTree {val op = "*"}
  case class Div(lhs: ExprTree, rhs: ExprTree) extends ArithmeticOperatorTree {val op = "/"}
  case class Modulo(lhs: ExprTree, rhs: ExprTree) extends ArithmeticOperatorTree {val op = "%"}

  case class LogicAnd(lhs: ExprTree, rhs: ExprTree) extends LogicalOperatorTree {val op = "&"}
  case class LogicOr(lhs: ExprTree, rhs: ExprTree) extends LogicalOperatorTree {val op = "|"}
  case class LogicXor(lhs: ExprTree, rhs: ExprTree) extends LogicalOperatorTree {val op = "^"}

  case class LeftShift(lhs: ExprTree, rhs: ExprTree) extends ShiftOperatorTree {val op = "<<"}
  case class RightShift(lhs: ExprTree, rhs: ExprTree) extends ShiftOperatorTree {val op = ">>"}

  case class LessThan(lhs: ExprTree, rhs: ExprTree) extends ComparisonOperatorTree {val op = "<"}
  case class LessThanEquals(lhs: ExprTree, rhs: ExprTree) extends ComparisonOperatorTree {val op = "<="}
  case class GreaterThan(lhs: ExprTree, rhs: ExprTree) extends ComparisonOperatorTree {val op = ">"}
  case class GreaterThanEquals(lhs: ExprTree, rhs: ExprTree) extends ComparisonOperatorTree {val op = ">="}

  case class Equals(lhs: ExprTree, rhs: ExprTree) extends EqualsOperatorTree {val op = "=="}
  case class NotEquals(lhs: ExprTree, rhs: ExprTree) extends EqualsOperatorTree {val op = "!="}

  case class And(lhs: ExprTree, rhs: ExprTree) extends BinaryOperatorTree {val op = "&&"}
  case class Or(lhs: ExprTree, rhs: ExprTree) extends BinaryOperatorTree {val op = "||"}

  /*-------------------------------- Unary Operator Trees --------------------------------*/

  trait UnaryOperatorTree extends OperatorTree {
    val expr: ExprTree
    def operatorString(args: List[Any]) = op + args.head
  }

  object UnaryOperatorTree {
    def unapply(e: UnaryOperatorTree): Option[(ExprTree)] = e match {
      case e: UnaryOperatorTree => Some(e.expr)
      case _                    => None
    }
  }

  trait IncrementDecrementTree extends UnaryOperatorTree
  object IncrementDecrementTree {
    def unapply(e: IncrementDecrementTree): Option[(ExprTree)] = e match {
      case e: IncrementDecrementTree => Some(e.expr)
      case _                         => None
    }
  }

  case class Not(expr: ExprTree) extends UnaryOperatorTree {val op = "!"}
  case class Hash(expr: ExprTree) extends UnaryOperatorTree {val op = "#"}
  case class Negation(expr: ExprTree) extends UnaryOperatorTree {val op = "-"}
  case class LogicNot(expr: ExprTree) extends UnaryOperatorTree {val op = "~"}

  case class PreIncrement(expr: ExprTree) extends IncrementDecrementTree {val op = "++"}
  case class PreDecrement(expr: ExprTree) extends IncrementDecrementTree {val op = "--"}
  case class PostIncrement(expr: ExprTree) extends IncrementDecrementTree {
    val op = "++"
    override def operatorString(args: List[Any]) = args.head + op
  }
  case class PostDecrement(expr: ExprTree) extends IncrementDecrementTree {
    val op = "--"
    override def operatorString(args: List[Any]) = args.head + op
  }

  /*-------------------------------- Array Operator Trees --------------------------------*/

  trait ArrayOperatorTree extends OperatorTree {
    val arr: ExprTree
    def operatorString(args: List[Any], className: String): String = className + operatorString(args)
  }

  case class ArrayAssign(arr: ExprTree, index: ExprTree, expr: ExprTree) extends ArrayOperatorTree {
    override val op: String = "[]="
    override def operatorString(args: List[Any]): String = s"[${args(0)}] = ${args(1)}"
  }
  case class ArrayRead(arr: ExprTree, index: ExprTree) extends ArrayOperatorTree {
    override val op: String = "[]"
    override def operatorString(args: List[Any]): String = s"[${args(0)}]"
  }
  case class ArraySlice(arr: ExprTree, start: Option[ExprTree], end: Option[ExprTree]) extends ArrayOperatorTree {
    override val op: String = "[:]"
    override def operatorString(args: List[Any]): String = s"[${args(0)}:${args(1)}]"
  }

  /*-------------------------------- Literal and Identifer Trees --------------------------------*/


  trait Literal[T] extends ExprTree with LeafTree {
    val value: T
  }

  object Literal {
    def unapply(e: Literal[_]): Option[Any] = e match {
      case e: Literal[_] => Some(e.value)
      case _             => None
    }
  }

  case class IntLit(value: Int) extends Literal[Int]
  case class LongLit(value: Long) extends Literal[Long]
  case class FloatLit(value: Float) extends Literal[Float]
  case class DoubleLit(value: Double) extends Literal[Double]
  case class CharLit(value: Char) extends Literal[Char]
  case class StringLit(value: String) extends Literal[String]
  case class ArrayLit(value: List[ExprTree]) extends ExprTree
  case class True() extends ExprTree with LeafTree
  case class False() extends ExprTree with LeafTree
  case class Null() extends ExprTree with LeafTree

  trait IdentifierTree[T <: Symbol] extends ExprTree with Symbolic[T] {
    val value: String
  }

  case class Identifier(value: String) extends IdentifierTree[Symbol] with LeafTree {
    // The type of the identifier depends on the type of the symbol
    override def getType: Type = {
      if (!hasSymbol)
        return TUntyped

      getSymbol match {
        case cs: ClassSymbol    => TObject(cs)
        case vs: VariableSymbol => vs.getType
        case ms: MethodSymbol   => ms.getType
        case es: ErrorSymbol    => TError
      }
    }

    override def setType(tpe: Type) = {
      if (hasSymbol)
        getSymbol.setType(tpe)
      this
    }
  }

  case class ClassIdentifier(value: String, var templateTypes: List[TypeTree] = List()) extends IdentifierTree[ClassSymbol] with TypeTree {

    import tcompiler.modification.Templates._

    val name = value

    // The type of the identifier depends on the type of the symbol
    override def getType: Type = if (hasSymbol) TObject(getSymbol) else TUntyped

    override def setType(tpe: Type) = {
      if (hasSymbol)
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

      val s = value.split("::")
      val prefix = if (s.size == 1)
                     ""
                   else
                     s.dropRight(1).mkString("::") + "::"
      val name = s.last
      prefix + StartEnd + name + (if (isTemplated) Seperator + tTypes.mkString(Seperator)) + StartEnd
    }
  }

  /*-------------------------------- Expression Trees --------------------------------*/

  case class Assign(id: Identifier, expr: ExprTree) extends ExprTree
  case class FieldAssign(obj: ExprTree, id: Identifier, expr: ExprTree) extends ExprTree
  case class FieldAccess(var obj: ExprTree, id: Identifier) extends ExprTree
  case class This() extends ExprTree with Symbolic[ClassSymbol] with LeafTree
  case class Super(specifier: Option[Identifier]) extends ExprTree with Symbolic[ClassSymbol]
  case class NewArray(var tpe: TypeTree, sizes: List[ExprTree]) extends ExprTree {def dimension = sizes.size}
  case class New(var tpe: TypeTree, args: List[ExprTree]) extends ExprTree
  case class Ternary(condition: ExprTree, thn: ExprTree, els: ExprTree) extends ExprTree
  case class Instance(expr: ExprTree, id: Identifier) extends ExprTree
  case class As(expr: ExprTree, var tpe: TypeTree) extends ExprTree
  case class MethodCall(var obj: ExprTree, meth: Identifier, args: List[ExprTree]) extends ExprTree

  case class Empty() extends ExprTree with LeafTree {override def toString = "<EMPTY>"}

  def isStaticCall(obj: ExprTree) =
    obj match {
      case id@Identifier(name) =>
        id.getSymbol match {
          case _: ClassSymbol => true
          case _              => false
        }
      case _                   => false
    }

  /**
    * Statements that have no effect on their own.
    */
  object UselessStatement {
    def unapply(e: StatTree): Option[ExprTree] = e match {
      case _: MethodCall             => None
      case IncrementDecrementTree(_) => None
      case _: Assign |
           _: ArrayAssign |
           _: FieldAssign            => None
      case expr: ExprTree            => Some(expr)
      case _                         => None
    }
  }

}
