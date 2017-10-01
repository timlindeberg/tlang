package tlang.compiler
package ast

import tlang.Constants
import tlang.compiler.analyzer.Symbols._
import tlang.compiler.analyzer.Types
import tlang.compiler.analyzer.Types._
import tlang.compiler.imports.Imports
import tlang.formatting.{DefaultFormatting, SimpleFormatting}
import tlang.utils.Extensions._
import tlang.utils.{FillTreeHelpers, Positioned}

import scala.collection.{TraversableLike, mutable}

@FillTreeHelpers
object Trees {


  private val noColorPrinter = PrettyPrinter(SimpleFormatting)
  private val colorPrinter   = PrettyPrinter(DefaultFormatting)

  trait Tree extends Positioned with Product with TraversableLike[Tree, List[Tree]] {

    override protected[this] def newBuilder: mutable.Builder[Tree, List[Tree]] = new mutable.ListBuffer
    override def seq: TraversableOnce[Tree] = this

    override def foreach[U](f: Tree => U): Unit = {
      val traverser = new Trees.Traverser {
        override def _traverse(t: Tree): Unit = {
          f(t)
          super._traverse(t)
        }
      }
      traverser.traverse(this)
    }

    def copyAttributes(t: Tree): this.type = {
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
    override def toString: String = noColorPrinter(this)

    private def copySymbolTrees[T <: Symbol, U <: Symbol](to: Tree, from: Tree): Unit = {
      if (to.getClass != from.getClass)
        return

      if (!from.isInstanceOf[Symbolic[_]] || !from.asInstanceOf[Symbolic[_]].hasSymbol)
        return

      // This is not very elegant but is one way to get around type erasure
      from match {
        case x: ClassDecl      => to.asInstanceOf[ClassDecl].setSymbol(x.getSymbol)
        case x: TraitDecl      => to.asInstanceOf[TraitDecl].setSymbol(x.getSymbol)
        case x: ExtensionDecl  => to.asInstanceOf[ExtensionDecl].setSymbol(x.getSymbol)
        case x: ClassID        => to.asInstanceOf[ClassID].setSymbol(x.getSymbol)
        case x: VariableID     => to.asInstanceOf[VariableID].setSymbol(x.getSymbol)
        case x: MethodID       => to.asInstanceOf[MethodID].setSymbol(x.getSymbol)
        case x: MethodDeclTree => to.asInstanceOf[MethodDeclTree].setSymbol(x.getSymbol)
        case x: Formal         => to.asInstanceOf[Formal].setSymbol(x.getSymbol)
        case x: VarDecl        => to.asInstanceOf[VarDecl].setSymbol(x.getSymbol)
        case x: This           => to.asInstanceOf[This].setSymbol(x.getSymbol)
        case x: Super          => to.asInstanceOf[Super].setSymbol(x.getSymbol)
        case x: New            => to.asInstanceOf[New].setSymbol(x.getSymbol)
        case _                 => ???
      }
    }

    def prettyPrint: this.type = { println(colorPrinter(this)); this }

    override def clone: this.type = {
      val cloner = new Trees.Transformer {
        override val treeCopy = new Trees.Copier
        def transformation: PartialFunction[Tree, Tree] = Map.empty
      }
      cloner(this)
    }

    def children: List[Tree] = {
      val x = productIterator.flatMap {
        case l: Traversable[_] if l.nonEmpty && l.head.isInstanceOf[Tree] => l.asInstanceOf[Traversable[Tree]]
        case o: Option[_] if o.nonEmpty && o.get.isInstanceOf[Tree]       => o.get.asInstanceOf[Tree] :: Nil
        case t: Tree                                                      => t :: Nil
        case _                                                            => Nil
      }
      x.toList
    }

  }

  // Signals that the node is a leaf and no further recursion is necessary
  trait Leaf

  /*---------------------------- Top level Trees ----------------------------*/

  case class CompilationUnit(pack: Package,
    var classes: List[ClassDeclTree],
    imports: Imports) extends Tree {

    def packageName: String = pack.address.mkString("::")

    override def children: List[Tree] = imports.imports ::: classes

  }

  case class Annotation() extends Tree

  /*------------------------ Package and Import Trees -----------------------*/

  case class Package(address: List[String] = Nil) extends Tree with Leaf {
    override val isEmpty: Boolean = address.isEmpty
    val name: String = address.mkString("::")
  }

  object Import {
    def unapply(i: Import) = Some(i.address)
  }

  trait Import extends Tree with Leaf {
    val address: List[String]

    def name: String = address.mkString("::")
    def writtenName: String = name

    def shortName: String = address.last

  }

  case object RegularImport {
    def apply(fullName: String) = new RegularImport(fullName)
  }

  case class RegularImport(address: List[String]) extends Import {
    def this(fullName: String) = this(fullName.split("::").toList)
  }

  case class WildCardImport(address: List[String]) extends Import {
    override def writtenName: String = super.name + "::*"
  }
  case class ExtensionImport(address: List[String], className: List[String]) extends Import {

    override def name: String = ((address :+ ExtensionDecl.seperator) ::: className).mkString("::")
    override def writtenName: String = ((address :+ "extension") ::: className).mkString("::")

  }

  /*------------------------ Class Declaration Trees ------------------------*/


  object ClassDeclTree {
    def unapply(c: ClassDeclTree) = Some(c.tpe, c.parents, c.fields, c.methods)
  }

  object IDClassDeclTree {
    def unapply(c: IDClassDeclTree) = Some(c.id, c.parents, c.fields, c.methods)
  }

  trait ClassDeclTree extends Tree with Symbolic[ClassSymbol] {
    val tpe    : TypeTree
    var parents: List[ClassID]
    val fields : List[VarDecl]
    var methods: List[MethodDeclTree]

    def isAbstract: Boolean

    def traits: List[ClassID] = parents.filter(_.getSymbol.isAbstract)
  }

  trait IDClassDeclTree extends ClassDeclTree {
    val id: ClassID
    override val tpe: ClassID = id
  }


  case class ClassDecl(id: ClassID,
    var parents: List[ClassID] = Nil,
    fields: List[VarDecl] = Nil,
    var methods: List[MethodDeclTree] = Nil) extends IDClassDeclTree {

    val isAbstract = false
  }

  case class TraitDecl(id: ClassID,
    var parents: List[ClassID] = Nil,
    fields: List[VarDecl] = Nil,
    var methods: List[MethodDeclTree] = Nil) extends IDClassDeclTree {
    val isAbstract = true
  }

  object ExtensionDecl {
    def stripExtension(fullName: String): String = fullName.replaceAll(""".*\$EX::""", "")
    val seperator = "$EX"
  }

  case class ExtensionDecl(tpe: TypeTree, var methods: List[MethodDeclTree] = Nil) extends ClassDeclTree {
    // Extensions cannot declare parents or fields
    // Fields might be supported in the future
    var parents: List[ClassID] = List[ClassID]()
    val fields : List[VarDecl] = List[VarDecl]()
    val isAbstract             = false
  }

  /*----------------------------- Modifier Trees ----------------------------*/

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
    val isStatic     : Boolean       = modifiers.contains(Static())
    val isFinal      : Boolean       = modifiers.contains(Final())
    val accessability: Accessability = modifiers.findInstance[Accessability].getOrElse(Private())
  }

  /*----------------------- Function Declaration Trees ----------------------*/


  object MethodDeclTree {
    def unapply(f: MethodDeclTree) = Some(f.id, f.modifiers, f.args, f.retType, f.stat)

    def mainMethod(stat: StatTree): MethodDecl = mainMethod(List(stat))
    def mainMethod(stats: List[StatTree]): MethodDecl = mainMethod(Some(Block(stats)))
    def mainMethod(stat: Option[StatTree]): MethodDecl = mainMethod(stat, None)
    def mainMethod(stat: Option[StatTree], classSym: ClassSymbol): MethodDecl = mainMethod(stat, Some(classSym))

    private def mainMethod(stat: Option[StatTree], classSym: Option[ClassSymbol]): MethodDecl = {
      val modifiers: Set[Modifier] = Set(Public(), Static())
      val id = MethodID("main")
      val args = Formal(ArrayType(ClassID(Constants.JavaString, List())), VariableID("args")) :: Nil
      val retType = Some(UnitType())
      val meth = MethodDecl(id, modifiers, args, retType, stat)
      if (classSym.isDefined) {
        val mainSym = new MethodSymbol("main", classSym.get, stat, modifiers).setType(TUnit)
        val argsSym = new VariableSymbol("args").setType(TArray(String))
        mainSym.addArgument(argsSym)
        meth.setSymbol(mainSym)
      }
      meth
    }
  }

  trait MethodDeclTree extends Tree with Symbolic[MethodSymbol] with Modifiable {
    val id       : MethodID
    val args     : List[Formal]
    val retType  : Option[TypeTree]
    val stat     : Option[StatTree]
    val modifiers: Set[Modifier]

    def isMain: Boolean = this == MethodDeclTree.mainMethod(stat)

    def isAbstract: Boolean = stat.isEmpty

    // Does not contain return type. Can be used to compare methods.
    def signature: String = id.name + args.map(_.tpe.name).mkString("(", ", ", ")")

    // This can used for displaying the method and contains the return type as well
    def fullSignature: String = signature + retType.map(t => ": " + t.name).getOrElse("")

  }

  case class MethodDecl(
    id: MethodID,
    modifiers: Set[Modifier] = Set(),
    args: List[Formal] = Nil,
    retType: Option[TypeTree] = None,
    stat: Option[StatTree] = None
  ) extends MethodDeclTree

  case class ConstructorDecl(
    id: MethodID,
    modifiers: Set[Modifier] = Set(),
    args: List[Formal] = Nil,
    retType: Option[TypeTree] = None,
    stat: Option[StatTree] = None
  ) extends MethodDeclTree

  case class OperatorDecl(
    operatorType: OperatorTree,
    modifiers: Set[Modifier] = Set(),
    args: List[Formal] = Nil,
    retType: Option[TypeTree] = None,
    stat: Option[StatTree] = None)
    extends MethodDeclTree {
    val id: MethodID = MethodID("")
  }

  case class Formal(tpe: TypeTree, id: VariableID) extends Tree with Symbolic[VariableSymbol]

  /*---------------------------- Statement Trees ----------------------------*/

  trait StatTree extends Tree

  trait PrintStatTree extends StatTree {
    val expr: ExprTree
  }
  object PrintStatTree {
    def unapply(e: PrintStatTree) = Some(e.expr)
  }


  case class VarDecl(
    id: VariableID,
    tpe: Option[TypeTree] = None,
    initiation: Option[ExprTree] = None,
    modifiers: Set[Modifier] = Set())
    extends StatTree with Symbolic[VariableSymbol] with Modifiable

  case class Block(stats: List[StatTree]) extends StatTree
  case class If(condition: ExprTree, thn: StatTree, els: Option[StatTree]) extends StatTree
  case class While(condition: ExprTree, stat: StatTree) extends StatTree
  case class For(initiation: List[StatTree], condition: ExprTree, post: List[StatTree], stat: StatTree) extends StatTree
  case class Foreach(varDecl: VarDecl, container: ExprTree, stat: StatTree) extends StatTree

  case class Error(expr: ExprTree) extends StatTree
  case class Return(expr: Option[ExprTree]) extends StatTree with Typed
  case class Break() extends StatTree with Leaf
  case class Continue() extends StatTree with Leaf

  case class Print(expr: ExprTree) extends PrintStatTree
  case class Println(expr: ExprTree) extends PrintStatTree

  trait ExprTree extends StatTree with Typed

  /*------------------------------- Type Trees ------------------------------*/

  trait TypeTree extends Tree with Typed {
    val name: String
  }

  trait PrimitiveTypeTree extends TypeTree with Leaf

  case class ArrayType(tpe: TypeTree) extends TypeTree {val name: String = tpe.name + "[]" }
  case class NullableType(tpe: TypeTree) extends TypeTree {val name: String = tpe.name + "?" }
  case class UnitType() extends PrimitiveTypeTree {val name = "Unit" }

  /*------------------------- Binary Operator Trees -------------------------*/


  trait OperatorTree extends ExprTree {
    val opSign: String

    def signature(args: List[Any]): String

    def lookupOperator(arg: Type, imports: Imports): Option[OperatorSymbol] = lookupOperator(List(arg), imports)

    def lookupOperator(args: (Type, Type), imports: Imports): Option[OperatorSymbol] = lookupOperator(List(args._1, args._2), imports)

    def lookupOperator(args: List[Type], imports: Imports): Option[OperatorSymbol] = {
      args.foreach { arg =>
        lookupOperator(arg, args, imports) match {
          case Some(op) => return Some(op)
          case None     =>
        }
      }
      None
    }

    def lookupOperator(classType: Type, args: List[Type], imports: Imports): Option[OperatorSymbol] = {
      classType match {
        case TObject(classSymbol) => classSymbol.lookupOperator(this, args, imports)
        case _                    => None
      }
    }

  }

  trait BinaryOperatorTree extends OperatorTree {
    val lhs: ExprTree
    val rhs: ExprTree

    def signature(args: List[Any]): String = s"${ args(0) } $opSign ${ args(1) }"
  }

  object BinaryOperatorTree {
    def unapply(e: BinaryOperatorTree) = Some(e.lhs, e.rhs)
  }

  trait ArithmeticOperatorTree extends BinaryOperatorTree
  object ArithmeticOperatorTree {
    def unapply(e: ArithmeticOperatorTree) = Some(e.lhs, e.rhs)
  }

  trait ShiftOperatorTree extends BinaryOperatorTree
  object ShiftOperatorTree {
    def unapply(e: ShiftOperatorTree) = Some(e.lhs, e.rhs)
  }

  trait LogicalOperatorTree extends BinaryOperatorTree
  object LogicalOperatorTree {
    def unapply(e: LogicalOperatorTree) = Some(e.lhs, e.rhs)
  }

  case class Plus(lhs: ExprTree, rhs: ExprTree) extends ArithmeticOperatorTree {val opSign = "+" }
  case class Minus(lhs: ExprTree, rhs: ExprTree) extends ArithmeticOperatorTree {val opSign = "-" }
  case class Times(lhs: ExprTree, rhs: ExprTree) extends ArithmeticOperatorTree {val opSign = "*" }
  case class Div(lhs: ExprTree, rhs: ExprTree) extends ArithmeticOperatorTree {val opSign = "/" }
  case class Modulo(lhs: ExprTree, rhs: ExprTree) extends ArithmeticOperatorTree {val opSign = "%" }

  case class LogicAnd(lhs: ExprTree, rhs: ExprTree) extends LogicalOperatorTree {val opSign = "&" }
  case class LogicOr(lhs: ExprTree, rhs: ExprTree) extends LogicalOperatorTree {val opSign = "|" }
  case class LogicXor(lhs: ExprTree, rhs: ExprTree) extends LogicalOperatorTree {val opSign = "^" }

  case class LeftShift(lhs: ExprTree, rhs: ExprTree) extends ShiftOperatorTree {val opSign = "<<" }
  case class RightShift(lhs: ExprTree, rhs: ExprTree) extends ShiftOperatorTree {val opSign = ">>" }

  /*------------------------ Branching Operator Trees -----------------------*/

  trait BranchingOperatorTree extends OperatorTree

  trait ComparisonOperatorTree extends BranchingOperatorTree with BinaryOperatorTree
  object ComparisonOperatorTree {
    def unapply(e: ComparisonOperatorTree) = Some(e.lhs, e.rhs)
  }

  trait EqualsOperatorTree extends BranchingOperatorTree with BinaryOperatorTree
  object EqualsOperatorTree {
    def unapply(e: EqualsOperatorTree) = Some(e.lhs, e.rhs)
  }

  case class LessThan(lhs: ExprTree, rhs: ExprTree) extends ComparisonOperatorTree {val opSign = "<" }
  case class LessThanEquals(lhs: ExprTree, rhs: ExprTree) extends ComparisonOperatorTree {val opSign = "<=" }
  case class GreaterThan(lhs: ExprTree, rhs: ExprTree) extends ComparisonOperatorTree {val opSign = ">" }
  case class GreaterThanEquals(lhs: ExprTree, rhs: ExprTree) extends ComparisonOperatorTree {val opSign = ">=" }

  case class Equals(lhs: ExprTree, rhs: ExprTree) extends EqualsOperatorTree {val opSign = "==" }
  case class NotEquals(lhs: ExprTree, rhs: ExprTree) extends EqualsOperatorTree {val opSign = "!=" }

  case class And(lhs: ExprTree, rhs: ExprTree) extends BranchingOperatorTree with BinaryOperatorTree {val opSign = "&&" }
  case class Or(lhs: ExprTree, rhs: ExprTree) extends BranchingOperatorTree with BinaryOperatorTree {val opSign = "||" }

  /*-------------------------- Unary Operator Trees -------------------------*/

  trait UnaryOperatorTree extends OperatorTree {
    val expr: ExprTree

    def signature(args: List[Any]): String = opSign + args.head
  }

  object UnaryOperatorTree {
    def unapply(e: UnaryOperatorTree) = Some(e.expr)
  }

  trait IncrementDecrementTree extends UnaryOperatorTree {
    val isPre, isIncrement: Boolean
  }
  object IncrementDecrementTree {
    def unapply(e: IncrementDecrementTree) = Some(e.expr)
  }

  case class Not(expr: ExprTree) extends BranchingOperatorTree with UnaryOperatorTree {val opSign = "!" }
  case class Hash(expr: ExprTree) extends UnaryOperatorTree {val opSign = "#" }
  case class Negation(expr: ExprTree) extends UnaryOperatorTree {val opSign = "-" }
  case class LogicNot(expr: ExprTree) extends UnaryOperatorTree {val opSign = "~" }
  case class ExtractNullable(expr: ExprTree) extends UnaryOperatorTree {val opSign = "!!" }

  case class PreIncrement(expr: ExprTree) extends IncrementDecrementTree {
    val opSign      = "++"
    val isPre       = true
    val isIncrement = true
  }
  case class PreDecrement(expr: ExprTree) extends IncrementDecrementTree {
    val opSign      = "--"
    val isPre       = true
    val isIncrement = false
  }
  case class PostIncrement(expr: ExprTree) extends IncrementDecrementTree {
    val opSign      = "++"
    val isPre       = false
    val isIncrement = true
    override def signature(args: List[Any]): String = args.head + opSign
  }
  case class PostDecrement(expr: ExprTree) extends IncrementDecrementTree {
    val opSign      = "--"
    val isPre       = false
    val isIncrement = false
    override def signature(args: List[Any]): String = args.head + opSign
  }

  /*-------------------------- Array Operator Trees -------------------------*/

  trait ArrayOperatorTree extends OperatorTree {
    val arr: ExprTree

    def operatorString(args: List[Any], className: String): String = className + signature(args)
  }
  object ArrayOperatorTree {
    def unapply(e: ArrayOperatorTree) = Some(e.arr)
  }

  case class ArrayRead(arr: ExprTree, index: ExprTree) extends ArrayOperatorTree with Assignable {
    override val opSign: String = "[]"
    override def signature(args: List[Any]): String = s"[${ args(0) }]"
  }
  case class ArraySlice(arr: ExprTree, start: Option[ExprTree], end: Option[ExprTree], step: Option[ExprTree]) extends ArrayOperatorTree {
    override val opSign: String = "[::]"
    override def signature(args: List[Any]): String = s"[${ args(0) }:${ args(1) }]"
  }

  /*---------------------- Literal and Identifier Trees ---------------------*/


  trait Literal[T] extends ExprTree with Leaf {
    val value: T
  }
  trait NumberLiteral[T] extends Literal[T]

  object Literal {
    def unapply(e: Literal[_]): Option[Any] = Some(e.value)
  }


  case class IntLit(value: Int) extends NumberLiteral[Int] {override def getType: TObject = Types.Int }
  case class LongLit(value: Long) extends NumberLiteral[Long] {override def getType: TObject = Types.Long }
  case class FloatLit(value: Float) extends NumberLiteral[Float] {override def getType: TObject = Types.Float }
  case class DoubleLit(value: Double) extends NumberLiteral[Double] {override def getType: TObject = Types.Double }
  case class CharLit(value: Char) extends Literal[Char] {override def getType: TObject = Types.Char }
  case class StringLit(value: String) extends Literal[String] {override def getType: TObject = Types.String }
  case class TrueLit() extends Literal[Boolean] with Leaf {
    val value = true
    override def getType: TObject = Types.Bool
  }
  case class FalseLit() extends Literal[Boolean] with Leaf {
    val value = false
    override def getType: TObject = Types.Bool
  }
  case class NullLit() extends Literal[Null] with Leaf {
    val value: Null = null
    override def getType: Types.TNull.type = TNull
  }

  case class ArrayLit(value: List[ExprTree]) extends ExprTree

  trait Identifier[T <: Symbol] extends ExprTree with Symbolic[T] {
    val name: String

    // The type of the identifier depends on the type of the symbol
    override def getType: Type = {
      if (!hasSymbol)
        return TUntyped
      getSymbol.getType
    }

    // The type of the identifier depends on the type of the symbol
    override def setType(tpe: Type): Identifier.this.type = {
      if (hasSymbol)
        getSymbol.setType(tpe)
      this
    }
  }
  object Identifier {
    def unapply[T <: Symbol](e: Identifier[T]) = Some(e.name)
  }

  case class ClassID(name: String, templateTypes: List[TypeTree] = List()) extends Identifier[ClassSymbol] with TypeTree {

    import tlang.compiler.modification.Templating._

    // The type of the identifier depends on the type of the symbol
    override def getType: Type = if (hasSymbol) getSymbol.getType else TUntyped

    override def setType(tpe: Type): ClassID.this.type = this


    def isTemplated: Boolean = templateTypes.nonEmpty

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

  case class VariableID(name: String) extends Identifier[VariableSymbol] with Leaf with Assignable
  case class MethodID(name: String) extends Identifier[MethodSymbol] with Leaf

  /*------------------------------ Access Trees -----------------------------*/

  trait Access extends Assignable {
    var obj        : ExprTree
    val application: ExprTree

    def isStatic: Boolean = {
      if (obj.isInstanceOf[ClassID])
        return true

      application match {
        case id: VariableID if id.hasSymbol        => id.getSymbol.isStatic
        case MethodCall(meth, _) if meth.hasSymbol => meth.getSymbol.isStatic
        case _                                     => false
      }
    }
  }

  object Access {
    def unapply(e: Access) = Some(e.obj, e.application)
  }

  case class NormalAccess(var obj: ExprTree, application: ExprTree) extends Access
  case class SafeAccess(var obj: ExprTree, application: ExprTree) extends Access

  /*---------------------------- Expression Trees ---------------------------*/

  trait Assignable extends ExprTree

  case class Assign(to: Assignable, from: ExprTree) extends ArrayOperatorTree {
    override val arr   : ExprTree = to
    override val opSign: String   = "[]="
    override def signature(args: List[Any]): String = s"[${ args(0) }] = ${ args(1) }"
  }
  case class MethodCall(meth: MethodID, args: List[ExprTree]) extends ExprTree

  case class This() extends ExprTree with Symbolic[ClassSymbol] with Leaf
  case class Super(specifier: Option[ClassID]) extends ExprTree with Symbolic[ClassSymbol]
  case class NewArray(tpe: TypeTree, sizes: List[ExprTree]) extends ExprTree {
    def dimension: Int = sizes.size
  }
  case class New(tpe: TypeTree, args: List[ExprTree]) extends ExprTree with Symbolic[MethodSymbol]
  case class Ternary(condition: ExprTree, thn: ExprTree, els: ExprTree) extends ExprTree
  case class Elvis(nullableValue: ExprTree, ifNull: ExprTree) extends ExprTree
  case class Is(expr: ExprTree, tpe: TypeTree) extends ExprTree
  case class As(expr: ExprTree, tpe: TypeTree) extends ExprTree

  /*------------------------- Misc expression Trees -------------------------*/

  // Used to generate code which doesn't fit the tree structure but
  // which fits when generating bytecode. Can be used to transform an expression
  // in to multiple statements etc.
  // Generated when desugaring.
  case class GeneratedExpr(stats: List[StatTree]) extends ExprTree

  // Expression that will be compiled if a value is to be left
  // on the stack.
  case class PutValue(exprTree: ExprTree) extends ExprTree {
    override def getType: Type = exprTree.getType
  }

  // Used as a placeholder
  case class Empty() extends ExprTree with Leaf {override def toString = "<EMPTY>" }

  // Statements that have no effect on their own.
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


  type TreeTransformation = PartialFunction[Tree, Tree]


  trait Transformer {

    val treeCopy: Copier = new LazyCopier()

    def transformation: TreeTransformation

    final def transformChildren(t: Tree): Tree = ??? // Filled by FillTreeHelpers macro

    final def apply[T <: Tree](t: T): T = _transform(t).asInstanceOf[T]


    final def transform[T <: Tree](list: List[T]): List[T] = _transform(list).asInstanceOf[List[T]]
    final def transform[T <: Tree](set: Set[T]): Set[T] = _transform(set).asInstanceOf[Set[T]]
    final def transform[T <: Tree](op: Option[T]): Option[T] = _transform(op).asInstanceOf[Option[T]]

    final private def _transform(t: Tree): Tree = transformation.applyOrElse(t, transformChildren)
    final private def _transform[T <: Tree](list: List[T]): List[Tree] = smartMap(list).asInstanceOf[List[Tree]]
    final private def _transform[T <: Tree](set: Set[T]): Set[Tree] = smartMap(set).asInstanceOf[Set[Tree]]
    final private def _transform[T <: Tree](op: Option[T]): Option[Tree] = op match {
      case Some(t) =>
        val x = _transform(t)
        if (x eq t) op else Some(x)
      case None    => None
    }

    // This is used so we don't create new lists and sets when there is no change
    // to an element. This allows us to reuse larger parts of the tree and reduce allocation.
    private def smartMap[T <: Tree](traversable: Traversable[T]): Traversable[Tree] = {
      var anyDifferent = false
      val newSet = traversable map { t =>
        val x = _transform(t)
        if (!(t eq x))
          anyDifferent = true
        x
      }
      if (anyDifferent) newSet else traversable
    }
  }

  class Copier // Filled by FillTreeHelpers macro
  class LazyCopier extends Copier // Filled by FillTreeHelpers macro

  trait Traverser {

    protected def _traverse(t: Tree): Unit = ??? // Filled by FillTreeHelpers macro

    final def traverse(t: Tree): Unit = _traverse(t)
    final def traverse(trees: Traversable[Tree]): Unit = _traverse(trees)

    final protected def _traverse(op: Option[Tree]): Unit = op foreach _traverse
    final protected def _traverse(trees: Traversable[Tree]): Unit = trees foreach _traverse
  }


}
