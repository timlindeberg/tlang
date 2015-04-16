package koolc
package analyzer

import Symbols._

object Types {
  trait Typed {
    self =>

    private var _tpe: Type = TUntyped

    def setType(tpe: Type): self.type = { _tpe = tpe; this }
    def getType: Type = _tpe
  }

  sealed abstract class Type {
    def isSubTypeOf(tpe: Type): Boolean
  }

  case object TError extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = true
    override def toString = "[error]"
  }

  
  case object TUntyped extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = false
    override def toString = "[untyped]"
  }

  case object TInt extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = tpe match {
      case TInt => true
      case _    => false
    }
    override def toString = "int"
  }

  // TODO: Complete by creating necessary types
  case object TString extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = tpe match {
      case TString => true
      case _       => false
    }
    override def toString = "string"
  }

  case object TBool extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = tpe match {
      case TBool => true
      case _     => false
    }
    override def toString = "bool"
  }

  case object TIntArray extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = tpe match {
      case TIntArray => true
      case _         => false
    }
    override def toString = "int[]"
  }

  case class TObject(classSymbol: ClassSymbol) extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = tpe match {
      case TObject(c) =>
        if (classSymbol.name == c.name) {
          true
        } else {
          if (classSymbol.parent.isDefined) {
            classSymbol.parent.get.getType.isSubTypeOf(tpe)
          } else {
            false
          }
        }
      case _ => false
    }
    override def toString = classSymbol.name
    def ==(other: TObject): Boolean = classSymbol.name == other.classSymbol.name
  }

  // special object to implement the fact that all objects are its subclasses
  val anyObject = TObject(new ClassSymbol("Object"))
}
