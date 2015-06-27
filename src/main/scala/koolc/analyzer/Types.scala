package koolc
package analyzer

import Symbols._
import koolc.ast.Trees._
import koolc.code.CodeGeneration

object Types {
  trait Typed {
    self =>

    private var _tpe: Type = TUntyped

    def setType(tpe: Type): self.type = { _tpe = tpe; this }
    def getType: Type = _tpe
  }

  sealed abstract class Type {
    def isSubTypeOf(tpe: Type): Boolean = tpe.isInstanceOf[this.type]
    def getSuperTypes: List[Type] = List()

    def byteCodeName(): String
    val codes: CodeMap
  }

  case object TError extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = true
    override def toString = "[error]"
    override def byteCodeName(): String = "ERROR"
    override val codes = EmptyCodeMap
  }

  case object TUntyped extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = false
    override def toString = "[untyped]"
    override def byteCodeName(): String = "UNTYPED"
    override val codes = EmptyCodeMap
  }

  case object TUnit extends Type {
    override def toString = "Unit"
    override def byteCodeName(): String = "V"
    override val codes = EmptyCodeMap
  }

  case object TInt extends Type {
    override def toString = "Int"
    override def byteCodeName(): String = "I"
    override val codes = IntCodeMap
  }

  case object TBool extends Type {
    override def toString = "Bool"
    override def byteCodeName(): String = "Z"
    override val codes = BoolCodeMap
  }

  case object TString extends Type {
    override def toString = "String"
    override def byteCodeName(): String = "Ljava/lang/String;"
    override val codes = StringCodeMap
  }

  case class TArray(tpe: Type) extends Type {
    override def isSubTypeOf(otherTpe: Type): Boolean = {
      otherTpe match {
        case TArray(arrTpe) => tpe.isSubTypeOf(arrTpe)
        case _ => false
      }
    }
    override def toString = tpe.toString + "[]"
    override def byteCodeName(): String = "[" + tpe.byteCodeName
    override val codes = ArrayCodeMap
  }

  case class TObject(classSymbol: ClassSymbol) extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = tpe match {
      case obj @ TObject(c) =>
        if (classSymbol.name == c.name || c.name == anyObject.classSymbol.name) true
        else classSymbol.parent match {
          case Some(x) => x.getType.isSubTypeOf(tpe)
          case None    => false
        }
      case _ => false
    }
    override def getSuperTypes: List[Type] =
      List(this) ++ (classSymbol.parent match {
        case Some(parentSymbol) => parentSymbol.getType.getSuperTypes
        case None => List()
      })

    override def toString = classSymbol.name
    override def byteCodeName(): String = {
      val name = if (this == anyObject) CodeGeneration.OBJECT else classSymbol.name
      "L" + name + ";"
    }
    def ==(other: TObject): Boolean = classSymbol.name == other.classSymbol.name

    override val codes = new ObjectCodeMap(classSymbol.name)
  }

  // special object to implement the fact that all objects are its subclasses
  val anyObject = TObject(new ClassSymbol("Object"))
}
