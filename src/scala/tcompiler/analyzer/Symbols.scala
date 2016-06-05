package tcompiler
package analyzer

import tcompiler.analyzer.Types._
import tcompiler.ast.Trees._
import tcompiler.imports.ClassSymbolLocator
import tcompiler.utils._


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

  sealed abstract class Symbol extends Positioned with Typed

  val ObjectClass: ClassSymbol = new ClassSymbol("Object", false)
  ObjectClass.setType(Types.tObject)

  class GlobalScope {
    var classes = Map[String, ClassSymbol]()

    def lookupClass(prog: Program, name: String): Option[ClassSymbol] = {
      val fullName = getFullName(prog, name)
      classes.get(fullName) match {
        case Some(classSymbol) => Some(classSymbol)
        case None              =>
          ClassSymbolLocator.findSymbol(fullName) match {
            case Some(classSymbol) =>
              classes += fullName -> classSymbol
              Some(classSymbol)
            case None              => None
          }
      }
    }

    private def getFullName(prog: Program, name: String) = {
      val impMap = prog.importMap
      if (impMap.contains(name))
        impMap(name)
      else
        name
    }
  }

  class ClassSymbol(
    var name: String,
    var isAbstract: Boolean,
    var isComplete: Boolean = true) extends Symbol {

    name = name.replaceAll("\\.", "/")

    var parents: List[ClassSymbol] = Nil

    private var _methods: List[MethodSymbol] = Nil
    def methods = {
      completeClassSymbol()
      _methods
    }
    def methods_=(l: List[MethodSymbol]) = _methods = l

    private var _operators: List[OperatorSymbol] = Nil
    def operators = {
      completeClassSymbol()
      _operators
    }
    def operators_=(l: List[OperatorSymbol]) = _operators = l

    private var _fields = Map[String, VariableSymbol]()
    def fields = {
      completeClassSymbol()
      _fields
    }

    var writtenName = ""

    def fields_=(m: Map[String, VariableSymbol]) = _fields = m

    def addOperator(operatorSymbol: OperatorSymbol): Unit = operators = operatorSymbol :: operators

    def addMethod(methodSymbol: MethodSymbol): Unit = methods = methodSymbol :: methods

    def addField(varSymbol: VariableSymbol): Unit = fields += (varSymbol.name -> varSymbol)

    def isImplementedInSuperClass(name: String, args: List[Type]) = {
      findMethod(name, args) match {
        case Some(m) => false
        case None    => true
      }
    }

    def implementsMethod(method: MethodSymbol): Boolean = {
      findMethod(method.name, method.argTypes) match {
        case Some(meth) if !meth.isAbstract => true
        case None                           => parents.exists(p => p.implementsMethod(method))
        case _                              => false
      }
    }

    def lookupParentMethod(name: String, args: List[Type]): Option[MethodSymbol] = {
      val matchingMethods = parents.map(_.lookupMethod(name, args)).filter(_.isDefined)
      if (matchingMethods.isEmpty)
        None
      else
        matchingMethods.head // TODO: Conflicting error
    }

    def lookupParent(name: String): Option[ClassSymbol] = {
      parents.find(_.name == name) match {
        case v@Some(p) => v
        case None      =>
          parents.foreach(p => p.lookupParent(name) match {
            case v@Some(p) => return v
            case None      =>
          })
          None
      }
    }

    def lookupMethod(name: String, args: List[Type], recursive: Boolean = true): Option[MethodSymbol] = {
      findMethod(name, args) match {
        case Some(meth)                            => Some(meth)
        case None if recursive && parents.nonEmpty => lookupParentMethod(name, args)
        case _                                     => None
      }
    }

    def lookupParentVar(name: String): Option[VariableSymbol] = {
      val matchingVars = parents.map(_.lookupVar(name)).filter(_.isDefined)
      if (matchingVars.isEmpty)
        None
      else
        matchingVars.head
    }

    def lookupVar(name: String, recursive: Boolean = true): Option[VariableSymbol] = {
      fields.get(name) match {
        case x@Some(_)                             => x
        case None if recursive && parents.nonEmpty =>
          val matchingVars = parents.map(_.lookupVar(name))
          matchingVars.head
        case _                                     => None
      }
    }

    def lookupOperator(operatorType: OperatorTree, args: List[Type], recursive: Boolean = true): Option[OperatorSymbol] = {
      findOperator(operatorType, args) match {
        case x@Some(_)                             => x
        case None if recursive && parents.nonEmpty =>
          val matchingOperators = parents.map(_.lookupOperator(operatorType, args))
          matchingOperators.head
        case _                                     => None
      }
    }


    def unimplementedMethods(): List[(MethodSymbol, ClassSymbol)] =
      methods.filter(_.isAbstract).map((_, this)) ::: parents.flatMap(_.unimplementedMethods())


    private[Symbols] def completeClassSymbol() =
      if (!isComplete) {
        ClassSymbolLocator.fillClassSymbol(this)
        isComplete = true
      }

    private def findOperator(operatorType: OperatorTree, args: List[Type]): Option[OperatorSymbol] =
      operators.find(symbol => {
        sameOperatorType(operatorType, symbol.operatorType) &&
          args.size == symbol.argList.size &&
          args.zip(symbol.argList.map(_.getType)).forall { case (arg1, arg2) => arg1.isSubTypeOf(arg2) }
      })

    private def findMethod(name: String, args: List[Type]) =
      methods.find(symbol =>
        name == symbol.name &&
          hasMatchingArgumentList(symbol, args)
      )

    private def hasMatchingArgumentList(symbol: MethodSymbol, args: List[Type]): Boolean = {
      if (args.size != symbol.argList.size)
        return false

      if (args.size == 1) {
        val methodArg = symbol.argList.head.getType
        val expectedArg = args.head
        expectedArg.isSubTypeOf(methodArg) || methodArg.isImplicitlyConvertableFrom(expectedArg)
      } else {
        args.zip(symbol.argList.map(_.getType)).forall { case (arg1, arg2) => arg1.isSubTypeOf(arg2) }
      }
    }

    private def sameOperatorType(operatorType1: ExprTree, operatorType2: ExprTree) = {
      operatorType1 match {
        case _: PreIncrement | _: PostIncrement => operatorType2.isInstanceOf[PreIncrement] || operatorType2.isInstanceOf[PostIncrement]
        case _: PreDecrement | _: PostDecrement => operatorType2.isInstanceOf[PreDecrement] || operatorType2.isInstanceOf[PostDecrement]
        case _                                  => operatorType1.getClass == operatorType2.getClass
      }
    }
  }

  class MethodSymbol(
    val name: String,
    val classSymbol: ClassSymbol,
    val stat: Option[StatTree],
    val modifiers: Set[Modifier]) extends Symbol with Modifiable {

    var isAbstract                       = stat.isEmpty
    var params                           = Map[String, VariableSymbol]()
    var members                          = Map[String, VariableSymbol]()
    var argList   : List[VariableSymbol] = Nil
    var overridden: Option[MethodSymbol] = None

    def lookupVar(name: String): Option[VariableSymbol] = lookupLocalVar(name) match {
      case x@Some(_) => x
      case None      => lookupField(name)
    }

    def lookupField(name: String): Option[VariableSymbol] = classSymbol.lookupVar(name)
    def lookupArgument(name: String): Option[VariableSymbol] = params.get(name)

    def lookupArgument(index: Int): VariableSymbol = argList(index)

    def lookupLocalVar(name: String): Option[VariableSymbol] = members.get(name) match {
      case x@Some(_) => x
      case None      => params.get(name)
    }

    def argTypes = argList.map(_.getType)

    def signature = name + argList.map(_.name).mkString("(", ", ", ")")

    def byteCodeSignature = {
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

      if (modifiers.size != 2 || !modifiers.contains(Static()) || !modifiers.contains(Public()))
        return false

      true
    }

  }

  class OperatorSymbol(
    val operatorType: OperatorTree,
    override val classSymbol: ClassSymbol,
    override val stat: Option[StatTree],
    override val modifiers: Set[Modifier]
  ) extends MethodSymbol(operatorType.toString, classSymbol, stat, modifiers) {

    def operatorString = operatorType.operatorString(argList.map(_.getType))

    def methodName = "$" + operatorType.getClass.getSimpleName

  }

  trait VariableType
  case object Field extends VariableType
  case object Argument extends VariableType
  case object LocalVar extends VariableType

  class VariableSymbol(val name: String, val varType: VariableType = LocalVar, val modifiers: Set[Modifier] = Set(), val classSymbol: Option[ClassSymbol] = None) extends Symbol with Modifiable

  case class ErrorSymbol(name: String = "") extends Symbol

}
