package tcompiler
package analyzer

import tcompiler.analyzer.Types._
import tcompiler.ast.TreeGroups.{BinaryOperatorDecl, UnaryOperatorDecl}
import tcompiler.ast.Trees._
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

  sealed abstract class Symbol extends Positioned with Typed {
    val id: Int = ID.next
    val name: String
  }

  object ID {

    def reset() = c = 0

    private var c: Int = 0

    def next: Int = {
      val ret = c
      c = c + 1
      ret
    }
  }

  val objectClass = new ClassSymbol("Object").setType(Types.anyObject)

  class GlobalScope {
    var mainClass: ClassSymbol = _
    var classes = Map[String, ClassSymbol]("Object" -> objectClass)

    def lookupClass(n: String): Option[ClassSymbol] = classes.get(n)
  }

  class ClassSymbol(val name: String) extends Symbol {
    var parent: Option[ClassSymbol] = None
    var methods: List[MethodSymbol] = Nil
    var operators: List[OperatorSymbol] = Nil
    var members = Map[String, VariableSymbol]()


    def addOperator(operatorSymbol: OperatorSymbol): Unit = operators = operatorSymbol :: operators

    def addMethod(methodSymbol: MethodSymbol): Unit = methods = methodSymbol :: methods

    def lookupMethod(name: String, args: List[Type], recursive: Boolean = true): Option[MethodSymbol] = findMethod(name, args) match {
      case x @ Some(_) => x
      case None        => if (recursive && parent.isDefined) parent.get.lookupMethod(name, args) else None
    }

    def lookupVar(name: String, recursive: Boolean = true): Option[VariableSymbol] = members.get(name) match {
      case x @ Some(_) => x
      case None        => if (recursive && parent.isDefined) parent.get.lookupVar(name) else None
    }

    def lookupOperator(operatorType: ExprTree, args: List[Type], recursive: Boolean = true): Option[OperatorSymbol] =
      findOperator(operatorType, args) match {
        case x @ Some(_) => x
        case None        => if (recursive && parent.isDefined) parent.get.lookupOperator(operatorType, args) else None
      }

    private def findOperator(operatorType: ExprTree, args: List[Type]): Option[OperatorSymbol] =
      operators.find(symbol => {
        sameOperatorType(operatorType, symbol.operatorType) &&
          args.size == symbol.argList.size &&
          args.zip(symbol.argList.map(_.getType)).forall { case (arg1, arg2) => arg1.isSubTypeOf(arg2) }
      })

    private def findMethod(name: String, args: List[Type]) = {
      methods.find(symbol => {
        name == symbol.name &&
          args.size == symbol.argList.size &&
          args.zip(symbol.argList.map(_.getType)).forall { case (arg1, arg2) => arg1.isSubTypeOf(arg2) }
      })
    }

    private def sameOperatorType(operatorType1: ExprTree, operatorType2: ExprTree) = {
      operatorType1 match {
        case _: PreIncrement | _: PostIncrement => operatorType2.isInstanceOf[PreIncrement] || operatorType2.isInstanceOf[PostIncrement]
        case _: PreDecrement | _: PostDecrement => operatorType2.isInstanceOf[PreDecrement] || operatorType2.isInstanceOf[PostDecrement]
        case _                                  => operatorType1.getClass == operatorType2.getClass
      }
    }
  }

  class MethodSymbol(val name: String, val classSymbol: ClassSymbol, val ast: FuncTree) extends Symbol with Modifiable {
    val modifiers = ast.modifiers
    var params = Map[String, VariableSymbol]()
    var members = Map[String, VariableSymbol]()
    var argList: List[VariableSymbol] = Nil
    var overridden: Option[MethodSymbol] = None

    def lookupVar(name: String): Option[VariableSymbol] = lookupLocalVar(name) match {
      case x @ Some(_) => x
      case None        => lookupField(name)
    }

    def lookupField(name: String): Option[VariableSymbol] = classSymbol.lookupVar(name)
    def lookupArgument(name: String): Option[VariableSymbol] = params.get(name)

    def lookupLocalVar(name: String): Option[VariableSymbol] = members.get(name) match {
      case x @ Some(_) => x
      case None        => params.get(name)
    }


    def signature = {
      val argTypes = argList.map(_.getType.byteCodeName).mkString
      "(" + argTypes + ")" + getType.byteCodeName
    }


  }

  class OperatorSymbol(val operatorType: ExprTree, override val classSymbol: ClassSymbol, override val ast: FuncTree)
    extends MethodSymbol(operatorType.toString, classSymbol, ast) {
    def methodName = {
      val name = operatorType.getClass.getSimpleName
      val types = argList.map(_.getType)
      name + (operatorType match {
        case UnaryOperatorDecl(_)     => "$" + types.head
        case BinaryOperatorDecl(_, _) => "$" + types.head + "$" + types(1)
        case ArrayRead(_, _)          => "$" + types.head
        case ArrayAssign(_, _, _)     => "$" + types.head + "$" + types(1)
      })
    }
  }

  class VariableSymbol(val name: String, val modifiers: Set[Modifier] = Set(), val classSymbol: Option[ClassSymbol] = None) extends Symbol with Modifiable

  case class ErrorSymbol(name: String = "") extends Symbol

}
