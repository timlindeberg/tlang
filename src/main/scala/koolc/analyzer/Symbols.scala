package koolc
package analyzer

import utils._
import koolc.ast.Trees._
import koolc.analyzer.Types._

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
    var methods = Map[String, MethodSymbol]()
    var members = Map[String, VariableSymbol]()

    def lookupMethod(n: String): Option[MethodSymbol] = methods.get(n) match {
      case x @ Some(t) => x
      case None        => if (parent.isDefined) parent.get.lookupMethod(n) else None
    }
    def lookupVar(n: String): Option[VariableSymbol] = members.get(n) match {
      case x @ Some(t) => x
      case None        => if (parent.isDefined) parent.get.lookupVar(n) else None
    }
  }

  class MethodSymbol(val name: String, val classSymbol: ClassSymbol) extends Symbol {
    var params = Map[String, VariableSymbol]()
    var members = Map[String, VariableSymbol]()
    var argList: List[VariableSymbol] = Nil
    var overridden: Option[MethodSymbol] = None

    def lookupVar(n: String): Option[VariableSymbol] = members.get(n) match {
      case x @ Some(t) => x
      case None => params.get(n) match {
        case x @ Some(t) => x
        case None        => classSymbol.lookupVar(n)
      }
    }
  }

  class VariableSymbol(val name: String) extends Symbol

  case class ErrorSymbol(val name: String = "") extends Symbol
}
