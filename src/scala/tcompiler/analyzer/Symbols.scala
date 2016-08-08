package tcompiler
package analyzer

import tcompiler.analyzer.Types._
import tcompiler.ast.Trees._
import tcompiler.imports.{ClassSymbolLocator, ImportMap}
import tcompiler.utils._
import tcompiler.utils.Extensions.MutableMapExtensions
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

    val classes = mutable.Map(Main.TLangObject.replaceAll("/", ".") -> Types.ObjectSymbol,
                               Main.TLangString.replaceAll("/", ".") -> Types.StringSymbol)

    def lookupClass(importMap: ImportMap, name: String): Option[ClassSymbol] = {
      val fullName = importMap.getFullName(name)
      classes.getOrElseMaybeUpdate(fullName, ClassSymbolLocator.findSymbol(fullName))
    }
  }

  class ClassSymbol(var name: String,
                    var isAbstract: Boolean,
                    var isComplete: Boolean = true) extends Symbol {

    name = name.replaceAll("\\.", "/")

    override def getType = TObject(this)
    override def setType(tpe: Type) = sys.error("Set type on ClassSymbol")

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

    private var _fields = Map[String, FieldSymbol]()
    def fields = {
      completeClassSymbol()
      _fields
    }
    def fields_=(m: Map[String, FieldSymbol]) = _fields = m

    def addOperator(operatorSymbol: OperatorSymbol): Unit = operators = operatorSymbol :: operators

    def addMethod(methodSymbol: MethodSymbol): Unit = methods = methodSymbol :: methods

    def addField(varSymbol: FieldSymbol): Unit = fields += (varSymbol.name -> varSymbol)

    def isImplementedInSuperClass(name: String, args: List[Type]) = {
      findMethod(name, args, exactTypes = false) match {
        case Some(m) => false
        case None    => true
      }
    }

    def implementsMethod(method: MethodSymbol): Boolean = {
      findMethod(method.name, method.argTypes, exactTypes = false) match {
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

    def lookupMethod(name: String, args: List[Type], recursive: Boolean = true, exactTypes: Boolean = false): Option[MethodSymbol] = {
      findMethod(name, args, exactTypes) match {
        case Some(meth)                            => Some(meth)
        case None if recursive && parents.nonEmpty => lookupParentMethod(name, args)
        case _                                     => None
      }
    }

    def lookupParentField(name: String): Option[FieldSymbol] = {
      val matchingVars = parents.map(_.lookupField(name)).filter(_.isDefined)
      if (matchingVars.isEmpty)
        None
      else
        matchingVars.head
    }

    def lookupField(name: String, recursive: Boolean = true): Option[FieldSymbol] = {
      fields.get(name) match {
        case x@Some(_)                             => x
        case None if recursive && parents.nonEmpty =>
          val matchingVars = parents.map(_.lookupField(name)).filter(_.isDefined)
          if(matchingVars.isEmpty)
            None
          else
            matchingVars.head
        case _                                     => None
      }
    }

    def lookupOperator(operatorType: OperatorTree, args: List[Type], exactTypes: Boolean = false): Option[OperatorSymbol] = {
      findOperator(operatorType, args, exactTypes) match {
        case x@Some(_)                => x
        case None if parents.nonEmpty =>
          val matchingOperators = parents.map(_.lookupOperator(operatorType, args, exactTypes)).filter(_.isDefined)
          if(matchingOperators.isEmpty)
            None
          else
            matchingOperators.head
        case _                        => None
      }
    }

    def unimplementedMethods(): List[(MethodSymbol, ClassSymbol)] =
      methods.filter(_.isAbstract).map((_, this)) ::: parents.flatMap(_.unimplementedMethods())

    override def toString = name

    private[Symbols] def completeClassSymbol() =
      if (!isComplete) {
        ClassSymbolLocator.fillClassSymbol(this)
        isComplete = true
      }

    private def findOperator(operatorType: OperatorTree, args: List[Type], exactTypes: Boolean): Option[OperatorSymbol] = {
      val ops = operators.filter(sym => sameOperatorType(operatorType, sym.operatorType))
      findMethodPrioritiseExactMatch(ops, name, args, exactTypes)
    }

    private def findMethod(name: String, args: List[Type], exactTypes: Boolean) = {
      val meths = methods.filter(_.name == name)
      findMethodPrioritiseExactMatch(meths, name, args, exactTypes)
    }

    private def findMethodPrioritiseExactMatch[T <: MethodSymbol](container: List[T],
                                                                  name: String,
                                                                  args: List[Type],
                                                                  exactTypes: Boolean) = {
      // Prioritise exact match
      container.find(sym => hasMatchingArgumentList(sym, args, exactTypes = true)) match {
        case Some(x) => Some(x)
        case None    =>
          if (!exactTypes)
            container.find(sym => hasMatchingArgumentList(sym, args, exactTypes = false))
          else
            None
      }
    }

    private def hasMatchingArgumentList(symbol: MethodSymbol, args: List[Type], exactTypes: Boolean): Boolean = {
      if (args.size != symbol.argList.size)
        return false

      args.zip(symbol.argList.map(_.getType)).forall {
        case (expectedArg, methodArg) =>
          if (exactTypes)
            expectedArg == methodArg
          else
            expectedArg.isSubTypeOf(methodArg) || methodArg.isImplicitlyConvertableFrom(expectedArg)
      }
    }

    private def sameOperatorType(operatorType1: ExprTree, operatorType2: ExprTree) = {
      operatorType1 match {
        case Assign(ArrayRead(_, _), _)         =>
          operatorType2.isInstanceOf[Assign] && operatorType2.asInstanceOf[Assign].to.isInstanceOf[ArrayRead]
        case _: PreIncrement | _: PostIncrement => operatorType2.isInstanceOf[PreIncrement] || operatorType2.isInstanceOf[PostIncrement]
        case _: PreDecrement | _: PostDecrement => operatorType2.isInstanceOf[PreDecrement] || operatorType2.isInstanceOf[PostDecrement]
        case _                                  => operatorType1.getClass == operatorType2.getClass
      }
    }
  }

  class MethodSymbol(val name: String,
                     val classSymbol: ClassSymbol,
                     val stat: Option[StatTree],
                     val modifiers: Set[Modifier]) extends Symbol with Modifiable {

    var isAbstract                       = stat.isEmpty
    var args                             = Map[String, VariableSymbol]()
    var argList   : List[VariableSymbol] = Nil
    var overridden: Option[MethodSymbol] = None

    def lookupField(name: String): Option[VariableSymbol] = classSymbol.lookupField(name)
    def lookupArgument(name: String): Option[VariableSymbol] = args.get(name)

    def lookupArgument(index: Int): VariableSymbol = argList(index)

    def argTypes = argList.map(_.getType)

    def signature = name + argTypes.mkString("(", ", ", ")")

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

      if (modifiers.size != 2 || !isStatic || accessability != Public())
        return false

      true
    }

    override def toString = signature


  }

  class OperatorSymbol(val operatorType: OperatorTree,
                       override val classSymbol: ClassSymbol,
                       override val stat: Option[StatTree],
                       override val modifiers: Set[Modifier]
                      ) extends MethodSymbol("$" + operatorType.getClass.getSimpleName, classSymbol, stat, modifiers) {

    def operatorString = operatorType.operatorString(argList.map(_.getType))

    override def toString = operatorString

  }

  class VariableSymbol(val name: String,
                       val modifiers: Set[Modifier] = Set()) extends Symbol with Modifiable {
    override def toString = name
  }

  class FieldSymbol(override val name: String,
                    override val modifiers: Set[Modifier] = Set(),
                    val classSymbol: ClassSymbol) extends VariableSymbol(name, modifiers) with Modifiable

  case object ErrorSymbol extends Symbol { val name = Errors.ErrorName }

}
