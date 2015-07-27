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
    var classes                = Map[String, ClassSymbol]("Object" -> objectClass)

    def lookupClass(n: String): Option[ClassSymbol] = classes.get(n)
  }

  class MethodMap extends scala.collection.mutable.HashMap[(String, List[Type]), MethodSymbol] {
    protected override def elemEquals(key1: (String, List[Type]), key2: (String, List[Type])): Boolean = {
      // TODO: This fails for some cases for some reason
      val (name1, args1) = key1
      val (name2, args2) = key2

      if (name1 == name2 && args1.size == args2.size) {
        args1.zip(args2).forall { case (arg1, arg2) => arg2.isSubTypeOf(arg1) }
      } else {
        false
      }
    }
  }

  private class OperatorKey(val operatorType: ExprTree, val args: List[Type]) {
    def equals(other: OperatorKey): Boolean = {
      if (operatorType.getClass != other.operatorType.getClass)
        return false

      if (args.size != other.args.size)
        return false

      args.zip(other.args).forall { case (arg1, arg2) => arg2.isSubTypeOf(arg1) }
    }

    override def hashCode(): Int = {
      operatorType.getClass.hashCode + args.hashCode
    }
  }


  class ClassSymbol(val name: String) extends Symbol {
    var parent: Option[ClassSymbol] = None
    var methods                     = new MethodMap()
    private val operators = new scala.collection.mutable.HashMap[OperatorKey, OperatorSymbol]()
    var members = Map[String, VariableSymbol]()

    def addOperator(operatorType: ExprTree, args: List[Type], operatorSymbol: OperatorSymbol): Unit =
      operators += new OperatorKey(operatorType, args) -> operatorSymbol

    def lookupMethod(name: String, args: List[Type]): Option[MethodSymbol] = methods.get((name, args)) match {
      case x @ Some(_) => x
      case None        => if (parent.isDefined) parent.get.lookupMethod(name, args) else None
    }
    def lookupVar(name: String): Option[VariableSymbol] = members.get(name) match {
      case x @ Some(_) => x
      case None        => if (parent.isDefined) parent.get.lookupVar(name) else None
    }
    def lookupOperator(operatorType: ExprTree, args: List[Type]): Option[OperatorSymbol] = {
      operators.get(new OperatorKey(operatorType, args)) match {
        case x @ Some(_) => x
        case None        => None // operators can't be inherited
      }
    }
  }

  class MethodSymbol(val name: String, val classSymbol: ClassSymbol, val access: Accessability) extends Symbol {
    var params                           = Map[String, VariableSymbol]()
    var members                          = Map[String, VariableSymbol]()
    var argList   : List[VariableSymbol] = Nil
    var overridden: Option[MethodSymbol] = None

    def lookupVar(name: String): Option[VariableSymbol] = members.get(name) match {
      case x @ Some(t) => x
      case None        => params.get(name) match {
        case x @ Some(t) => x
        case None        => classSymbol.lookupVar(name)
      }
    }

    def signature = {
      val argTypes = argList.map(_.getType.byteCodeName).mkString
      "(" + argTypes + ")" + getType.byteCodeName
    }
  }

  class OperatorSymbol(val operatorType: ExprTree, override val classSymbol: ClassSymbol, override val access: Accessability)
    extends MethodSymbol(operatorType.toString, classSymbol, access) {
    def methodName = {
      val name = operatorType.getClass.getName
      name + (operatorType match {
        case UnaryOperatorDecl(_)     => "$" + argList(0)
        case BinaryOperatorDecl(_, _) => "$" + argList(0) + "$" + argList(1)
      })
    }
  }

  class VariableSymbol(val name: String, val access: Accessability = Public) extends Symbol

  case class ErrorSymbol(name: String = "") extends Symbol
}
