package tcompiler
package analyzer

import Symbols._
import tcompiler.code.{CodeGenerator, CodeGeneration}

object Types {

  trait Typed {
    self =>

    private var _tpe: Type = TUntyped

    def setType(tpe: Type): self.type = {
      _tpe = tpe
      this
    }

    def getType: Type = _tpe
  }

  val primitives = List[Type](
    TInt,
    TLong,
    TFloat,
    TDouble,
    TChar)

  sealed abstract class Type {
    def isSubTypeOf(tpe: Type): Boolean = tpe.isInstanceOf[this.type]
    def getSuperTypes: List[Type] = List()
    def isPrimitive = primitives.contains(this)

    def byteCodeName: String
    val codes: CodeMap
    val size: Int
  }

  case object TError extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = true
    override def toString = "[error]"
    override def byteCodeName: String = "ERROR"
    override val codes = EmptyCodeMap
    override val size: Int = 0
  }

  case object TUntyped extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = false
    override def toString = "[untyped]"
    override def byteCodeName: String = "UNTYPED"
    override val codes = EmptyCodeMap
    override val size: Int = 0
  }

  case object TUnit extends Type {
    override def toString = "Unit"
    override def byteCodeName: String = "V"
    override val codes = EmptyCodeMap
    override val size: Int = 0
  }

  case object TInt extends Type {
    override def toString = "Int"
    override def byteCodeName: String = "I"
    override val codes = IntCodeMap
    override val size: Int = 1
  }

  case object TLong extends Type {
    override def toString = "Long"
    override def byteCodeName: String = "J"
    override val codes = LongCodeMap
    override val size: Int = 2
  }

  case object TFloat extends Type {
    override def toString = "Float"
    override def byteCodeName: String = "F"
    override val codes = FloatCodeMap
    override val size: Int = 1
  }

  case object TDouble extends Type {
    override def toString = "Double"
    override def byteCodeName: String = "D"
    override val codes = DoubleCodeMap
    override val size: Int = 2
  }

  case object TChar extends Type {
    override def toString = "Char"
    override def byteCodeName: String = "C"
    override val codes = CharCodeMap
    override val size: Int = 1
  }

  case object TBool extends Type {
    override def toString = "Bool"
    override def byteCodeName: String = "Z"
    override val codes = BoolCodeMap
    override val size: Int = 1
  }

  case object TString extends Type {
    override def toString = "String"
    override def byteCodeName: String = "Ljava/lang/String;"
    override val codes = StringCodeMap
    override val size: Int = 1
  }

  case class TArray(tpe: Type) extends Type {
    override def isSubTypeOf(otherTpe: Type): Boolean = otherTpe match {
      case TArray(arrTpe) => tpe.isSubTypeOf(arrTpe)
      case _              => false
    }
    override def toString = tpe.toString + "[]"
    override def byteCodeName: String = "[" + tpe.byteCodeName
    override val codes = ArrayCodeMap
    override val size: Int = 1
  }

  case class TObject(classSymbol: ClassSymbol) extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = tpe match {
      case obj @ TObject(c) =>
        if (classSymbol.name == c.name || c.name == anyObject.classSymbol.name) true
        else classSymbol.parent match {
          case Some(x) => x.getType.isSubTypeOf(tpe)
          case None    => false
        }
      case _                => false
    }
    override def getSuperTypes: List[Type] =
      List(this) ++ (classSymbol.parent match {
        case Some(parentSymbol) => parentSymbol.getType.getSuperTypes
        case None               => List()
      })

    override def toString = classSymbol.name
    override def byteCodeName: String = {
      val name = if (this == anyObject) CodeGenerator.JavaObject else classSymbol.name
      "L" + name + ";"
    }
    def ==(other: TObject): Boolean = classSymbol.name == other.classSymbol.name

    override val codes = new ObjectCodeMap(classSymbol.name)
    override val size: Int = 1
  }

  // special object to implement the fact that all objects are its subclasses
  val anyObject = TObject(new ClassSymbol("Object"))
}
