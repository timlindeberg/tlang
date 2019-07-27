package tlang
package compiler
package analyzer

import tlang.compiler.analyzer.Types._
import tlang.compiler.ast.Trees._
import tlang.compiler.imports.{ClassSymbolLocator, Imports}
import tlang.compiler.messages.CompilerMessage
import tlang.utils.Positioned

import scala.collection.mutable

object Symbols {

  trait Symbolic[S <: Symbol] {
    private var _sym: Option[S] = None

    def hasSymbol: Boolean = _sym.isDefined

    def setSymbol(sym: S): this.type = {
      _sym = Some(sym)
      this
    }

    def getSymbol: S = _sym match {
      case Some(s) => s
      case None    => sys.error("Accessing undefined symbol.")
    }
  }

  sealed abstract class Symbol extends Positioned with Typed {
    def name: String
    def repr: String = System.identityHashCode(this).toHexString
  }

  class GlobalScope(classSymbolLocator: ClassSymbolLocator) {
    val classes: mutable.Map[String, ClassSymbol] =
      mutable.Map() ++ Types.DefaultTypes.map(tpe => tpe.name -> tpe.classSymbol).toMap

    def lookupClass(imports: Imports, name: String): Option[ClassSymbol] = {
      val fullName = imports.getFullName(name)
      classes.getOrElseMaybeUpdate(fullName) { classSymbolLocator.findSymbol(fullName) }
    }

    def classNames: List[String] = classes.keys.toList
  }

  object ClassErrorSymbol extends ClassSymbol(CompilerMessage.ErrorName)

  class ClassSymbol(override val name: String) extends Symbol {
    protected var _parents    : List[ClassSymbol]        = Nil
    protected var _methods    : List[MethodSymbol]       = Nil
    protected var _operators  : List[OperatorSymbol]     = Nil
    protected var _fields     : Map[String, FieldSymbol] = Map()
    protected var _isAbstract : Boolean                  = false
    protected var _annotations: List[AnnotationSymbol]   = Nil


    def parents: List[ClassSymbol] = _parents
    def methods: List[MethodSymbol] = _methods
    def operators: List[OperatorSymbol] = _operators
    def fields: Map[String, FieldSymbol] = _fields
    def annotations: List[AnnotationSymbol] = _annotations
    def isAbstract: Boolean = _isAbstract
    def isAbstract_=(isAbstract: Boolean): this.type = { _isAbstract = isAbstract; this }

    def addOperator(operator: OperatorSymbol): Unit = _operators ::= operator
    def addMethod(method: MethodSymbol): Unit = _methods ::= method
    def addField(field: FieldSymbol): Unit = _fields += (field.name -> field)
    def addParent(parent: ClassSymbol): Unit = _parents ::= parent
    def addAnnotation(annotation: AnnotationSymbol): Unit = _annotations ::= annotation

    def parents_=(parents: List[ClassSymbol]): Unit = _parents = parents

    def constructors: List[MethodSymbol] = methods.filter { _.name == "new" }

    def implicitConstructors: List[MethodSymbol] =
      constructors.filter { method =>
        method.modifiers.contains(Implicit()) &&
          method.argList.lengthCompare(1) == 0
      }

    def implementingMethod(abstractMeth: MethodSymbol): Option[MethodSymbol] =
      findMethod(abstractMeth.name, abstractMeth.argTypes, exactTypes = true)
        .filter(concreteMeth => !concreteMeth.isAbstract && concreteMeth.getType.isSubTypeOf(abstractMeth.getType))
        .orElse(parents.findDefined(_.implementingMethod(abstractMeth)))

    def overriddenMethod(concreteMeth: MethodSymbol): Option[MethodSymbol] =
      parents.findDefined(_.findMethod(concreteMeth.name, concreteMeth.argTypes, exactTypes = true))
        .filter(abstractMeth => abstractMeth.isAbstract && concreteMeth.getType.isSubTypeOf(abstractMeth.getType))
        .orElse(parents.findDefined(_.overriddenMethod(concreteMeth)))

    def implementsMethod(abstractMethod: MethodSymbol): Boolean = implementingMethod(abstractMethod).isDefined
    def overridesMethod(concreteMethod: MethodSymbol): Boolean = overriddenMethod(concreteMethod).isDefined

    def lookupParent(name: String): Option[ClassSymbol] =
      parents.find(_.name == name).orElse(parents.findDefined(_.lookupParent(name)))

    def lookupParentMethod(name: String, args: List[Type], imports: Imports, exactTypes: Boolean = false): Option[MethodSymbol] =
      parents.findDefined(_.lookupMethod(name, args, imports, exactTypes))

    def lookupParentOperator(operatorType: OperatorTree, args: List[Type], imports: Imports, exactTypes: Boolean = false): Option[OperatorSymbol] =
      parents.findDefined(_.lookupOperator(operatorType, args, imports, exactTypes))

    def lookupParentField(name: String): Option[FieldSymbol] =
      parents.findDefined(_.lookupField(name))

    def lookupMethod(name: String, args: List[Type], imports: Imports, exactTypes: Boolean = false): Option[MethodSymbol] =
      findMethod(name, args, exactTypes)
        .orElse(findExtensionMethod(name, args, imports, exactTypes))
        .orElse(lookupParentMethod(name, args, imports, exactTypes))

    def lookupOperator(operatorType: OperatorTree, args: List[Type], imports: Imports, exactTypes: Boolean = false): Option[OperatorSymbol] =
      findOperator(operatorType, args, exactTypes)
        .orElse(findExtensionOperator(operatorType, args, imports, exactTypes))
        .orElse(lookupParentOperator(operatorType, args, imports, exactTypes))

    def lookupField(name: String): Option[FieldSymbol] =
      fields.get(name).orElse(lookupParentField(name))

    def abstractMethods(): List[(MethodSymbol, ClassSymbol)] = {
      methods.filter(_.isAbstract).map((_, this)) ::: parents.flatMap(_.abstractMethods())
    }

    def findOperator(operatorType: OperatorTree, args: List[Type], exactTypes: Boolean): Option[OperatorSymbol] = {
      val ops = operators.filter(sym => sameOperatorType(operatorType, sym.operatorType))
      findMethodPrioritiseExactMatch(ops, name, args, exactTypes)
    }

    def findMethod(name: String, args: List[Type], exactTypes: Boolean): Option[MethodSymbol] = {
      val meths = methods.filter(_.name == name)
      findMethodPrioritiseExactMatch(meths, name, args, exactTypes)
    }

    override def toString: String = name
    override def getType = TObject(this)
    override def setType(tpe: Type): Nothing = sys.error("Cannot set type on ClassSymbol")

    protected def findMethodPrioritiseExactMatch[T <: MethodSymbol](methods: List[T], name: String, args: List[Type], exactTypes: Boolean): Option[T] =
      methods.find(sym => hasMatchingArgumentList(sym, args, exactTypes = true))
        .orElse {
          if (exactTypes) None
          else methods.find(sym => hasMatchingArgumentList(sym, args, exactTypes = false))
        }

    protected def findExtensionMethod(methodName: String, args: List[Type], imports: Imports, exactTypes: Boolean = false): Option[MethodSymbol] =
      imports.getExtensionClasses(name).findDefined(ext => ext.findMethod(methodName, args, exactTypes))

    protected def findExtensionOperator(operatorType: OperatorTree, args: List[Type], imports: Imports, exactTypes: Boolean = false): Option[OperatorSymbol] =
      imports.getExtensionClasses(name).findDefined(ext => ext.findOperator(operatorType, args, exactTypes))

    protected def hasMatchingArgumentList(symbol: MethodSymbol, args: List[Type], exactTypes: Boolean): Boolean = {
      if (args.lengthCompare(symbol.argList.size) != 0)
        return false

      args.zip(symbol.argTypes).forall {
        case (expectedArg, methodArg) =>
          if (exactTypes)
            expectedArg == methodArg
          else
            expectedArg.isSubTypeOf(methodArg) || methodArg.isImplicitlyConvertibleFrom(expectedArg)
      }
    }

    protected def sameOperatorType(operatorType1: ExprTree, operatorType2: ExprTree): Boolean = {
      (operatorType1, operatorType2) match {
        case (Assign(ArrayRead(_, _), _), Assign(ArrayRead(_, _), _))                 => true
        case (_: PreIncrement | _: PostIncrement, _: PreIncrement | _: PostIncrement) => true
        case (_: PreDecrement | _: PostDecrement, _: PreDecrement | _: PostDecrement) => true
        case _                                                                        => operatorType1.getClass == operatorType2.getClass
      }
    }
  }

  class ExtensionClassSymbol(override val name: String) extends ClassSymbol(name) {
    private var _extendedType: Option[Type] = None

    def setExtendedType(tpe: Type): Unit = _extendedType = Some(tpe)
    def getExtendedType: Type = _extendedType.get

    override def lookupField(name: String): Option[FieldSymbol] = {
      classSymbol.flatMap(_.lookupField(name))
    }

    override def lookupMethod(name: String, args: List[Type], imports: Imports, exactTypes: Boolean = false): Option[MethodSymbol] =
      super.lookupMethod(name, args, imports, exactTypes).
        orElse(classSymbol.flatMap(_.lookupMethod(name, args, imports, exactTypes)))

    override def lookupOperator(operatorType: OperatorTree, args: List[Type], imports: Imports, exactTypes: Boolean = false): Option[OperatorSymbol] =
      super.lookupOperator(operatorType, args, imports, exactTypes).
        orElse(classSymbol.flatMap(_.lookupOperator(operatorType, args, imports, exactTypes)))

    private def classSymbol: Option[ClassSymbol] = _extendedType collect {
      case TObject(classSymbol) => classSymbol
    }
  }

  class MethodSymbol(
    val name: String,
    val classSymbol: ClassSymbol,
    val stat: Option[StatTree],
    val modifiers: Set[Modifier]) extends Symbol with Modifiable {

    var isAbstract : Boolean                     = stat.isEmpty
    var args       : Map[String, VariableSymbol] = Map[String, VariableSymbol]()
    var argList    : List[VariableSymbol]        = Nil
    var annotations: List[AnnotationSymbol]      = Nil

    def addArgument(arg: VariableSymbol): Unit = {
      args += arg.name -> arg
      argList :+= arg
    }

    def addAnnotation(annotation: AnnotationSymbol): Unit = annotations ::= annotation

    def lookupField(name: String): Option[VariableSymbol] = classSymbol.lookupField(name)
    def lookupArgument(name: String): Option[VariableSymbol] = args.get(name)

    def lookupArgument(index: Int): VariableSymbol = argList(index)

    def argTypes: List[Type] = argList.map(_.getType)

    def signature: String = name + argTypes.mkString("(", ", ", ")")

    def isMainMethod: Boolean = {
      // Since we can't know a lot about the method at all phases the definition
      // of a main method is quite loose
      if (name != "main")
        return false

      if (argList.lengthCompare(1) != 0 || argList.head.name != "args")
        return false

      if (modifiers.size != 2 || !isStatic || accessibility != Public())
        return false

      true
    }

    override def toString: String = signature
  }

  class OperatorSymbol(
    val operatorType: OperatorTree,
    override val classSymbol: ClassSymbol,
    override val stat: Option[StatTree],
    override val modifiers: Set[Modifier]
  ) extends MethodSymbol(operatorType.operatorName, classSymbol, stat, modifiers) {
    override def signature: String = operatorType.signature(argList.map(_.getType))

    override def toString: String = signature
  }

  object VariableErrorSymbol extends VariableSymbol(CompilerMessage.ErrorName)
  class VariableSymbol(val name: String,
    val modifiers: Set[Modifier] = Set()) extends Symbol with Modifiable {
    override def toString: String = name
  }

  class FieldSymbol(override val name: String,
    override val modifiers: Set[Modifier] = Set(),
    val classSymbol: ClassSymbol) extends VariableSymbol(name, modifiers) with Modifiable

  case object ErrorSymbol extends Symbol {val name: String = CompilerMessage.ErrorName }


  trait AnnotationValue

  case class IntAnnotationValue(value: Int) extends AnnotationValue
  case class LongAnnotationValue(value: Long) extends AnnotationValue
  case class FloatAnnotationValue(value: Float) extends AnnotationValue
  case class DoubleAnnotationValue(value: Double) extends AnnotationValue
  case class StringAnnotationValue(value: String) extends AnnotationValue

  // Currently only supports constant types
  case class AnnotationSymbol(name: String, elements: List[(String, AnnotationValue)] = Nil)

}
