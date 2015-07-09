package tcompiler
package analyzer

import utils._
import tcompiler.ast.Trees._
import tcompiler.analyzer.Types._

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

  class MethodMap extends scala.collection.mutable.HashMap[(String, List[Type]), MethodSymbol] {
    protected override def elemEquals(key1: (String, List[Type]), key2: (String, List[Type])): Boolean = {
      // TODO: This fails for some cases for some reason
      val (name1, args1) = key1
      val (name2, args2) = key2

      if(name1 == name2 && args1.size == args2.size){
        args1.zip(args2).forall { case(arg1, arg2) => arg2.isSubTypeOf(arg1) }
      } else{
        false
      }
    }
  }

  class ClassSymbol(val name: String) extends Symbol {
    var parent: Option[ClassSymbol] = None
    var methods = new MethodMap()
    var members = Map[String, VariableSymbol]()

    def lookupMethod(name: String, args: List[Type]): Option[MethodSymbol] = methods.get((name, args)) match {
      case x @ Some(t) => x
      case None        => if (parent.isDefined) parent.get.lookupMethod(name, args) else None
    }
    def lookupVar(name: String): Option[VariableSymbol] = members.get(name) match {
      case x @ Some(t) => x
      case None        => if (parent.isDefined) parent.get.lookupVar(name) else None
    }
  }

  class MethodSymbol(val name: String, val classSymbol: ClassSymbol, val access: Accessability) extends Symbol {
    var params = Map[String, VariableSymbol]()
    var members = Map[String, VariableSymbol]()
    var argList: List[VariableSymbol] = Nil
    var overridden: Option[MethodSymbol] = None

    def lookupVar(name: String): Option[VariableSymbol] = members.get(name) match {
      case x @ Some(t) => x
      case None => params.get(name) match {
        case x @ Some(t) => x
        case None        => classSymbol.lookupVar(name)
      }
    }
  }

  class VariableSymbol(val name: String) extends Symbol

  case class ErrorSymbol(name: String = "") extends Symbol
}
