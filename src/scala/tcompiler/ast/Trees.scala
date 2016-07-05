package tcompiler
package ast

import tcompiler.analyzer.Symbols._
import tcompiler.analyzer.Types
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

    def copyTree(): this.type = {
      val copier = new TreeTransformer {
        override val treeCopy = new TreeCopier()
      }
      copier.transform(this).asInstanceOf[this.type]
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

    // For easier debugging
    override def toString = Printer(this, printInColor = false)

    private def copySymbolTrees[T <: Symbol, U <: Symbol](to: Tree, from: Tree): Unit = {
      if (to.getClass != from.getClass)
        return

      if (!from.isInstanceOf[Symbolic[_]] || !from.asInstanceOf[Symbolic[_]].hasSymbol)
        return

      // This is not very elegant but is one way to get around type erasure
      from match {
        case x: ClassDecl  => to.asInstanceOf[ClassDecl].setSymbol(x.getSymbol)
        case x: ClassID    => to.asInstanceOf[ClassID].setSymbol(x.getSymbol)
        case x: VariableID => to.asInstanceOf[VariableID].setSymbol(x.getSymbol)
        case x: MethodID   => to.asInstanceOf[MethodID].setSymbol(x.getSymbol)
        case x: FuncTree   => to.asInstanceOf[FuncTree].setSymbol(x.getSymbol)
        case x: Formal     => to.asInstanceOf[Formal].setSymbol(x.getSymbol)
        case x: VarDecl    => to.asInstanceOf[VarDecl].setSymbol(x.getSymbol)
        case x: This       => to.asInstanceOf[This].setSymbol(x.getSymbol)
        case x: Super      => to.asInstanceOf[Super].setSymbol(x.getSymbol)
      }
    }
  }


  // Signals that the node is a leaf and no further recursion is necessary
  trait Leaf

  /*-------------------------------- Top level Trees --------------------------------*/

  case class CompilationUnit(
    pack: Package,
    var imports: List[Import],
    var classes: List[ClassDecl],
    importMap: mutable.Map[String, String]) extends Tree {

    def getPackageDirectory = pack.directory

    def getPackageName(name: String) = (pack.adress :+ name).mkString(".")

    def importNames = imports map importName

    def importName(imp: Import): String = getFullName(imp.name)

    def importName(typeId: ClassID): String = {
      val name = typeId.name.replaceAll("::", ".")
      getFullName(name)
    }

    def getFullName(name: String) = importMap.getOrElse(name, name).replaceAll("::", ".")

  }

  /*-------------------------------- Package and Import Trees --------------------------------*/

  case class Package(adress: List[String]) extends Tree with Leaf {
    val directory = adress.mkString("/")
    val name      = adress.mkString(".")
  }

  object Import {
    def unapply(i: Import): Option[List[String]] = i match {
      case i: Import => Some(i.adress)
      case _         => None
    }
  }

  trait Import extends Tree with Leaf {
    val adress: List[String]

    def name = adress.mkString(".")
    def shortName = adress.last
    def writtenName = adress.mkString("::")
  }

  case class RegularImport(adress: List[String]) extends Import
  case class WildCardImport(adress: List[String]) extends Import


  /*-------------------------------- Class Declaration Trees --------------------------------*/

  case class ClassDecl(
    var id: ClassID,
    var parents: List[ClassID],
    var fields: List[VarDecl],
    var methods: List[FuncTree],
    var isAbstract: Boolean) extends Tree with Symbolic[ClassSymbol] {
    def implementedTraits = parents.filter(_.getSymbol.isAbstract)
  }


  /*-------------------------------- Modifier Trees --------------------------------*/

  trait Modifier extends Tree with Leaf

  trait Accessability extends Modifier

  case class Public() extends Accessability
  case class Private() extends Accessability
  case class Protected() extends Accessability

  case class Static() extends Modifier
  case class Implicit() extends Modifier
  case class Final() extends Modifier

  trait Modifiable {
    val modifiers: Set[Modifier]
    val isStatic      = modifiers.contains(Static())
    val isFinal       = modifiers.contains(Final())
    val accessability = modifiers.find(_.isInstanceOf[Accessability]).getOrElse(Private()).asInstanceOf[Accessability]
  }

  /*-------------------------------- Function Declaration Trees --------------------------------*/


  object FuncTree {
    def unapply(f: FuncTree): Option[(MethodID, Option[TypeTree], List[Formal], Option[StatTree], Set[Modifier])] = f match {
      case f: FuncTree => Some(f.id, f.retType, f.args, f.stat, f.modifiers)
      case _           => None
    }
  }

  trait FuncTree extends Tree with Symbolic[MethodSymbol] with Modifiable {
    var id       : MethodID
    var retType  : Option[TypeTree]
    var args     : List[Formal]
    val stat     : Option[StatTree]
    val modifiers: Set[Modifier]

    def isMain = this match {
      case MethodDecl(Some(UnitType()), Identifier("main"), Formal(ArrayType(ClassID("java::lang::String", List())), _) :: Nil, _, _) =>
        modifiers.size == 2 &&
          modifiers.contains(Public()) &&
          modifiers.contains(Static()) &&
          args.head.id.name == "args"
      case _                                                                                                                          => false
    }

    def isAbstract = stat.isEmpty

    def signature = id.name + args.map(_.tpe.name).mkString("(", ", ", ")")

  }

  case class MethodDecl(var retType: Option[TypeTree],
    var id: MethodID,
    var args: List[Formal],
    stat: Option[StatTree],
    modifiers: Set[Modifier]) extends FuncTree
  case class ConstructorDecl(var retType: Option[TypeTree],
    var id: MethodID,
    var args: List[Formal],
    stat: Option[StatTree],
    modifiers: Set[Modifier]) extends FuncTree
  case class OperatorDecl(var operatorType: OperatorTree,
    var retType: Option[TypeTree],
    var args: List[Formal],
    stat: Option[StatTree],
    modifiers: Set[Modifier]) extends FuncTree {
    var id: MethodID = new MethodID("")
  }

  case class Formal(var tpe: TypeTree, id: VariableID) extends Tree with Symbolic[VariableSymbol]

  /*-------------------------------- Type Trees --------------------------------*/

  trait TypeTree extends Tree with Typed {
    val name: String
  }

  case class ArrayType(var tpe: TypeTree) extends TypeTree {val name = tpe.name + "[]"}
  case class NullableType(var tpe: TypeTree) extends TypeTree {val name = tpe.name + "?"}
  case class IntType() extends TypeTree with Leaf {val name = "Int"}
  case class LongType() extends TypeTree with Leaf {val name = "Long"}
  case class FloatType() extends TypeTree with Leaf {val name = "Float"}
  case class DoubleType() extends TypeTree with Leaf {val name = "Double"}
  case class BooleanType() extends TypeTree with Leaf {val name = "Bool"}
  case class CharType() extends TypeTree with Leaf {val name = "Char"}
  case class UnitType() extends TypeTree with Leaf {val name = "Unit"}

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


  case class VarDecl(var tpe: Option[TypeTree], var id: VariableID, init: Option[ExprTree], modifiers: Set[Modifier]) extends StatTree with Symbolic[VariableSymbol] with Modifiable
  case class Block(stats: List[StatTree]) extends StatTree
  case class If(expr: ExprTree, thn: StatTree, els: Option[StatTree]) extends StatTree
  case class While(expr: ExprTree, stat: StatTree) extends StatTree
  case class For(init: List[StatTree], condition: ExprTree, post: List[StatTree], stat: StatTree) extends StatTree
  case class Foreach(varDecl: VarDecl, container: ExprTree, stat: StatTree) extends StatTree

  case class Error(expr: ExprTree) extends StatTree
  case class Return(expr: Option[ExprTree]) extends StatTree
  case class Break() extends StatTree with Leaf
  case class Continue() extends StatTree with Leaf

  case class Print(expr: ExprTree) extends PrintStatTree
  case class Println(expr: ExprTree) extends PrintStatTree

  /*-------------------------------- Binary Operator Trees --------------------------------*/

  trait ExprTree extends StatTree with Typed


  trait OperatorTree extends ExprTree {
    val op: String
    def operatorString(args: List[Any]): String

    def lookupOperator(arg: Type): Option[OperatorSymbol] = lookupOperator(List(arg))
    def lookupOperator(args: (Type, Type)): Option[OperatorSymbol] = lookupOperator(List(args._1, args._2))
    def lookupOperator(args: List[Type]): Option[OperatorSymbol] = {
      args.foreach { arg =>
        lookupOperator(arg, args) match {
          case Some(op) => return Some(op)
          case None =>
        }
      }
      None
    }
    def lookupOperator(classType: Type, args: List[Type]) = {
      classType match {
        case TObject(classSymbol) => classSymbol.lookupOperator(this, args)
        case _                    => None
      }
    }

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

  trait IncrementDecrementTree extends UnaryOperatorTree {
    val isPre, isIncrement      : Boolean
  }
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

  case class PreIncrement(expr: ExprTree) extends IncrementDecrementTree {
    val op          = "++"
    val isPre       = true
    val isIncrement = true
  }
  case class PreDecrement(expr: ExprTree) extends IncrementDecrementTree {
    val op          = "--"
    val isPre       = true
    val isIncrement = false
  }
  case class PostIncrement(expr: ExprTree) extends IncrementDecrementTree {
    val op          = "++"
    val isPre       = false
    val isIncrement = true
    override def operatorString(args: List[Any]) = args.head + op
  }
  case class PostDecrement(expr: ExprTree) extends IncrementDecrementTree {
    val op          = "--"
    val isPre       = false
    val isIncrement = false
    override def operatorString(args: List[Any]) = args.head + op
  }

  /*-------------------------------- Array Operator Trees --------------------------------*/

  trait ArrayOperatorTree extends OperatorTree {
    val arr: ExprTree
    def operatorString(args: List[Any], className: String): String = className + operatorString(args)
  }
  object ArrayOperatorTree {
    def unapply(e: ArrayOperatorTree): Option[ExprTree] = e match {
      case e: ArrayOperatorTree => Some(e.arr)
      case _                    => None
    }
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


  trait Literal[T] extends ExprTree with Leaf {
    val value: T
  }

  object Literal {
    def unapply(e: Literal[_]): Option[Any] = e match {
      case e: Literal[_] => Some(e.value)
      case _             => None
    }
  }

  case class IntLit(value: Int) extends Literal[Int] {
    override def getType = TInt
  }
  case class LongLit(value: Long) extends Literal[Long] {
    override def getType = TLong
  }
  case class FloatLit(value: Float) extends Literal[Float] {
    override def getType = TFloat
  }
  case class DoubleLit(value: Double) extends Literal[Double] {
    override def getType = TDouble
  }
  case class CharLit(value: Char) extends Literal[Char] {
    override def getType = TChar
  }
  case class StringLit(value: String) extends Literal[String] {
    override def getType = Types.String
  }
  case class ArrayLit(value: List[ExprTree]) extends ExprTree
  case class TrueLit() extends Literal[Boolean] with Leaf {
    val value = true
    override def getType = TBool
  }
  case class FalseLit() extends Literal[Boolean] with Leaf {
    val value = false
    override def getType = TBool
  }
  case class NullLit() extends Literal[Null] with Leaf {
    val value = null
    override def getType = TNull
  }

  trait Identifier[T <: Symbol] extends ExprTree with Symbolic[T] {
    val name: String

    // The type of the identifier depends on the type of the symbol
    override def getType: Type = {
      if (!hasSymbol)
        return TUntyped
      getSymbol.getType
    }

    // The type of the identifier depends on the type of the symbol
    override def setType(tpe: Type) = {
      if (hasSymbol)
        getSymbol.setType(tpe)
      this
    }
  }
  object Identifier {
    def unapply[T <: Symbol](e: Identifier[T]): Option[String] = e match {
      case e: Identifier[T] => Some(e.name)
      case _                => None
    }
  }

  case class ClassID(name: String, var templateTypes: List[TypeTree] = List()) extends Identifier[ClassSymbol] with TypeTree {

    import tcompiler.modification.Templates._

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
        case x: ClassID if x.isTemplated => x.templatedClassName
        case x                           => x.name
      }

      val s = name.split("::")
      val prefix = if (s.size == 1)
                     ""
                   else
                     s.dropRight(1).mkString("::") + "::"
      prefix + StartEnd + s.last + (if (isTemplated) Seperator + tTypes.mkString(Seperator)) + StartEnd
    }
  }

  case class VariableID(name: String) extends Identifier[VariableSymbol] with Leaf
  case class MethodID(name: String) extends Identifier[MethodSymbol] with Leaf

  /*-------------------------------- Access Trees --------------------------------*/

  trait Access extends ExprTree {
    var obj        : ExprTree
    val application: ExprTree

    def isStatic: Boolean = {
      if (obj.isInstanceOf[ClassID])
        return true

      application match {
        case id: VariableID if id.hasSymbol           =>
          id.getSymbol.isStatic
        case MethodCall(meth, args) if meth.hasSymbol =>
          meth.getSymbol.isStatic
        case _                                        => false
      }
    }
  }

  object Access {
    def unapply(e: Access): Option[(ExprTree, ExprTree)] = e match {
      case e: Access => Some(e.obj, e.application)
      case _         => None
    }
  }

  case class NormalAccess(var obj: ExprTree, application: ExprTree) extends Access
  case class SafeAccess(var obj: ExprTree, application: ExprTree) extends Access

  /*-------------------------------- Expression Trees --------------------------------*/

  case class Assign(to: ExprTree, expr: ExprTree) extends ArrayOperatorTree {
    override val arr: ExprTree = to
    override val op : String   = "[]="
    override def operatorString(args: List[Any]): String = s"[${args(0)}] = ${args(1)}"
  }
  case class MethodCall(meth: MethodID, args: List[ExprTree]) extends ExprTree

  case class This() extends ExprTree with Symbolic[ClassSymbol] with Leaf
  case class Super(specifier: Option[ClassID]) extends ExprTree with Symbolic[ClassSymbol]
  case class NewArray(var tpe: TypeTree, sizes: List[ExprTree]) extends ExprTree {def dimension = sizes.size}
  case class New(var tpe: TypeTree, args: List[ExprTree]) extends ExprTree
  case class Ternary(condition: ExprTree, thn: ExprTree, els: ExprTree) extends ExprTree
  case class Elvis(nullableValue: ExprTree, ifNull: ExprTree) extends ExprTree
  case class Is(expr: ExprTree, id: ClassID) extends ExprTree
  case class As(expr: ExprTree, var tpe: TypeTree) extends ExprTree

  /*-------------------------------- Misc expression Trees --------------------------------*/

  // Used to generate code which doesn't fit the tree structure but
  // which fits well on the stack. Can be used to transform an expression
  // in to multiple statements etc.
  // Generated when desugaring.
  case class GeneratedExpr(stats: List[StatTree]) extends ExprTree

  // Expression that will be compiled if a value is to be left
  // on the stack.
  case class PutValue(exprTree: ExprTree) extends ExprTree {
    override def getType = exprTree.getType
  }

  // Usually used as a placeholder
  case class Empty() extends ExprTree with Leaf {override def toString = "<EMPTY>"}

  /**
    * Statements that have no effect on their own.
    */
  object UselessStatement {
    def unapply(e: StatTree): Option[ExprTree] = e match {
      case Access(_, MethodCall(_, _)) => None
      case IncrementDecrementTree(_)   => None
      case _: Assign                   => None
      case _: GeneratedExpr            => None
      case _: PutValue                 => None
      case expr: ExprTree              => Some(expr)
      case _                           => None
    }
  }

}
