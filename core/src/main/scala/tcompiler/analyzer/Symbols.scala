package tcompiler
package analyzer

import tcompiler.analyzer.Types._
import tcompiler.ast.Trees._
import tcompiler.error.Errors
import tcompiler.imports.{ClassSymbolLocator, ImportMap}
import tcompiler.utils.Extensions._
import tcompiler.utils._

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
  }

  class GlobalScope {

    val classes: mutable.Map[String, ClassSymbol] = mutable.Map[String, ClassSymbol]()

    def lookupClass(importMap: ImportMap, name: String): Option[ClassSymbol] = {
      val fullName = importMap.getFullName(name)
      classes.getOrElseMaybeUpdate(fullName, ClassSymbolLocator.findSymbol(fullName))
    }

    def classNames: List[String] = classes.keys.toList

  }

  class ClassSymbol(className: String,
    var isAbstract: Boolean,
    var isComplete: Boolean = true) extends Symbol {

    val name: String = className.replaceAll("\\.", "/")

    override def getType = TObject(this)
    override def setType(tpe: Type): Nothing = sys.error("Set type on ClassSymbol")

    var parents: List[ClassSymbol] = Nil

    private var _methods: List[MethodSymbol] = Nil
    def methods: List[MethodSymbol] = {
      completeClassSymbol()
      _methods
    }
    def methods_=(l: List[MethodSymbol]): Unit = _methods = l

    private var _operators: List[OperatorSymbol] = Nil
    def operators: List[OperatorSymbol] = {
      completeClassSymbol()
      _operators
    }
    def operators_=(l: List[OperatorSymbol]): Unit = _operators = l

    private var _fields = Map[String, FieldSymbol]()
    def fields: Map[String, FieldSymbol] = {
      completeClassSymbol()
      _fields
    }
    def fields_=(m: Map[String, FieldSymbol]): Unit = _fields = m

    def addOperator(operatorSymbol: OperatorSymbol): Unit = operators ::= operatorSymbol

    def addMethod(methodSymbol: MethodSymbol): Unit = methods ::= methodSymbol

    def addField(varSymbol: FieldSymbol): Unit = fields += (varSymbol.name -> varSymbol)

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

    def lookupParentMethod(name: String, args: List[Type], importMap: ImportMap, exactTypes: Boolean = false): Option[MethodSymbol] =
      parents.findDefined(_.lookupMethod(name, args, importMap, exactTypes))


    def lookupParentOperator(operatorType: OperatorTree, args: List[Type], importMap: ImportMap, exactTypes: Boolean = false): Option[OperatorSymbol] =
      parents.findDefined(_.lookupOperator(operatorType, args, importMap, exactTypes))


    def lookupParentField(name: String): Option[FieldSymbol] =
      parents.findDefined(_.lookupField(name))


    def lookupMethod(name: String, args: List[Type], importMap: ImportMap, exactTypes: Boolean = false): Option[MethodSymbol] =
      findMethod(name, args, exactTypes)
        .orElse(findExtensionMethod(name, args, importMap, exactTypes))
        .orElse(lookupParentMethod(name, args, importMap, exactTypes))
        .orElse(ObjectSymbol.findMethod(name, args, exactTypes))
        .orElse(ObjectSymbol.findExtensionMethod(name, args, importMap, exactTypes))

    def lookupOperator(operatorType: OperatorTree, args: List[Type], importMap: ImportMap, exactTypes: Boolean = false): Option[OperatorSymbol] =
      findOperator(operatorType, args, exactTypes)
        .orElse(findExtensionOperator(operatorType, args, importMap, exactTypes))
        .orElse(lookupParentOperator(operatorType, args, importMap, exactTypes))
        .orElse(ObjectSymbol.findOperator(operatorType, args, exactTypes))
        .orElse(ObjectSymbol.findExtensionOperator(operatorType, args, importMap, exactTypes))


    def lookupField(name: String): Option[FieldSymbol] =
      fields.get(name).orElse(lookupParentField(name))


    def abstractMethods(): List[(MethodSymbol, ClassSymbol)] =
      methods.filter(_.isAbstract).map((_, this)) ::: parents.flatMap(_.abstractMethods())

    def findOperator(operatorType: OperatorTree, args: List[Type], exactTypes: Boolean): Option[OperatorSymbol] = {
      val ops = operators.filter(sym => sameOperatorType(operatorType, sym.operatorType))
      findMethodPrioritiseExactMatch(ops, name, args, exactTypes)
    }

    def findMethod(name: String, args: List[Type], exactTypes: Boolean): Option[MethodSymbol] = {
      val meths = methods.filter(_.name == name)
      findMethodPrioritiseExactMatch(meths, name, args, exactTypes)
    }

    override def toString: String = name

    protected def completeClassSymbol(): Unit =
      if (!isComplete) {
        ClassSymbolLocator.fillClassSymbol(this)
        isComplete = true
      }

    protected def findMethodPrioritiseExactMatch[T <: MethodSymbol](methods: List[T], name: String, args: List[Type], exactTypes: Boolean): Option[T] =
      methods.find(sym => hasMatchingArgumentList(sym, args, exactTypes = true))
        .orElse {
          if (exactTypes) None
          else methods.find(sym => hasMatchingArgumentList(sym, args, exactTypes = false))
        }

    protected def findExtensionMethod(methodName: String, args: List[Type], importMap: ImportMap, exactTypes: Boolean = false): Option[MethodSymbol] =
      importMap.getExtensionClasses(name).findDefined(ext => ext.findMethod(methodName, args, exactTypes))

    protected def findExtensionOperator(operatorType: OperatorTree, args: List[Type], importMap: ImportMap, exactTypes: Boolean = false): Option[OperatorSymbol] =
      importMap.getExtensionClasses(name).findDefined(ext => ext.findOperator(operatorType, args, exactTypes))

    protected def hasMatchingArgumentList(symbol: MethodSymbol, args: List[Type], exactTypes: Boolean): Boolean = {
      if (args.size != symbol.argList.size)
        return false

      args.zip(symbol.argList.map(_.getType)).forall {
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

  class ExtensionClassSymbol(name: String) extends ClassSymbol(name, false, true) {
    var originalClassSymbol: Option[ClassSymbol] = None

    override def lookupField(name: String): Option[FieldSymbol] = originalClassSymbol.flatMap(_.lookupField(name))

    override def lookupMethod(name: String, args: List[Type], importMap: ImportMap, exactTypes: Boolean = false): Option[MethodSymbol] =
      super.lookupMethod(name, args, importMap, exactTypes).
        orElse(originalClassSymbol.flatMap(_.lookupMethod(name, args, importMap, exactTypes)))

    override def lookupOperator(operatorType: OperatorTree, args: List[Type], importMap: ImportMap, exactTypes: Boolean = false): Option[OperatorSymbol] =
      super.lookupOperator(operatorType, args, importMap, exactTypes).
        orElse(originalClassSymbol.flatMap(_.lookupOperator(operatorType, args, importMap, exactTypes)))

  }

  class MethodSymbol(val name: String,
    val classSymbol: ClassSymbol,
    val stat: Option[StatTree],
    val modifiers: Set[Modifier]) extends Symbol with Modifiable {

    var isAbstract       : Boolean                     = stat.isEmpty
    val isExtensionMethod: Boolean                     = classSymbol.isInstanceOf[ExtensionClassSymbol]
    var args             : Map[String, VariableSymbol] = Map[String, VariableSymbol]()
    var argList          : List[VariableSymbol]        = Nil
    var annotations      : List[String]                = Nil

    def lookupField(name: String): Option[VariableSymbol] = classSymbol.lookupField(name)
    def lookupArgument(name: String): Option[VariableSymbol] = args.get(name)

    def lookupArgument(index: Int): VariableSymbol = argList(index)

    def argTypes: List[Type] = argList.map(_.getType)

    def signature: String = name + argTypes.mkString("(", ", ", ")")

    def byteCodeSignature: String = {
      val types = argTypes.map(_.byteCodeName).mkString
      s"($types)${getType.byteCodeName}"
    }

    def isMainMethod: Boolean = {
      // Since we can't know a lot about the method at all stages the definition
      // of a main method is quite loose
      if (name != "main")
        return false

      if (argList.size != 1 || argList.head.name != "args")
        return false

      if (modifiers.size != 2 || !isStatic || accessability != Public())
        return false

      true
    }

    override def toString: String = signature


  }

  class OperatorSymbol(val operatorType: OperatorTree,
    override val classSymbol: ClassSymbol,
    override val stat: Option[StatTree],
    override val modifiers: Set[Modifier]
  ) extends MethodSymbol("$" + operatorType.getClass.getSimpleName, classSymbol, stat, modifiers) {

    override def signature: String = operatorType.signature(argList.map(_.getType))

    override def toString: String = signature

  }

  class VariableSymbol(val name: String,
    val modifiers: Set[Modifier] = Set()) extends Symbol with Modifiable {
    override def toString: String = name
  }

  class FieldSymbol(override val name: String,
    override val modifiers: Set[Modifier] = Set(),
    val classSymbol: ClassSymbol) extends VariableSymbol(name, modifiers) with Modifiable

  case object ErrorSymbol extends Symbol {val name = Errors.ErrorName}

}
