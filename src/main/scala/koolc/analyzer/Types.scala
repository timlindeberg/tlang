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
    def byteCodeName(): String
  }

  case object TError extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = true
    override def toString = "[error]"
    override def byteCodeName(): String = "ERROR"
  }

  
  case object TUntyped extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = false
    override def toString = "[untyped]"
    override def byteCodeName(): String = "UNTYPED"
  }

  case object TInt extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = tpe match {
      case TInt => true
      case _    => false
    }
    override def toString = "int"
    override def byteCodeName(): String = "I"
  }

  case object TString extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = tpe match {
      case TString => true
      case _       => false
    }
    override def toString = "string"
    override def byteCodeName(): String = "Ljava/lang/String;"
  }

  case object TBool extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = tpe match {
      case TBool => true
      case _     => false
    }
    override def toString = "bool"
    override def byteCodeName(): String = "Z"
  }

  case object TIntArray extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = tpe match {
      case TIntArray => true
      case _         => false
    }
    override def toString = "int[]"
    override def byteCodeName(): String = "[I"
  }

  case class TObject(classSymbol: ClassSymbol) extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = tpe match {
      case _ if tpe == anyObject => true
      case TObject(c) =>
         if (classSymbol.name == c.name) true
         else classSymbol.parent match {
           case Some(x) => x.getType.isSubTypeOf(tpe)
           case None => false
        }
      case _ => false
    }
    override def toString = classSymbol.name
    def ==(other: TObject): Boolean = classSymbol.name == other.classSymbol.name
    override def byteCodeName(): String = "L" + classSymbol.name + ";"
  }

  // special object to implement the fact that all objects are its subclasses
  val anyObject = TObject(new ClassSymbol("Object"))
}
