package tcompiler
package analyzer

import tcompiler.Main._
import tcompiler.analyzer.Symbols._
import tcompiler.error.Errors
import tcompiler.imports.ClassSymbolLocator._

object Types {


  trait Typed {
    self =>

    protected var _tpe: Type = TUntyped

    def setType(tpe: Type): self.type = {
      _tpe = tpe
      this
    }

    def setType(tpe: Typed): self.type = {
      _tpe = tpe.getType
      this
    }

    def getType: Type = _tpe
  }

  val IntSymbol    = new ClassSymbol(TInt, isAbstract = false)
  val LongSymbol   = new ClassSymbol(TLong, isAbstract = false)
  val FloatSymbol  = new ClassSymbol(TFloat, isAbstract = false)
  val DoubleSymbol = new ClassSymbol(TDouble, isAbstract = false)
  val CharSymbol   = new ClassSymbol(TChar, isAbstract = false)
  val BoolSymbol   = new ClassSymbol(TBool, isAbstract = false)
  val ObjectSymbol = new ClassSymbol(JavaObject, isAbstract = false)
  val StringSymbol = new ClassSymbol(JavaString, isAbstract = false)

  val Int    = TObject(IntSymbol)
  val Long   = TObject(LongSymbol)
  val Float  = TObject(FloatSymbol)
  val Double = TObject(DoubleSymbol)
  val Char   = TObject(CharSymbol)
  val Bool   = TObject(BoolSymbol)
  val Object = TObject(ObjectSymbol)
  val String = TObject(StringSymbol)
  val Array  = TArray(Object)

  val Primitives: List[TObject] = List(Int, Long, Float, Double, Char, Bool)

  fillClassSymbol(IntSymbol)
  fillClassSymbol(LongSymbol)
  fillClassSymbol(FloatSymbol)
  fillClassSymbol(DoubleSymbol)
  fillClassSymbol(CharSymbol)
  fillClassSymbol(BoolSymbol)
  fillClassSymbol(ObjectSymbol)
  fillClassSymbol(StringSymbol)

  sealed abstract class Type {
    def isNullable: Boolean

    def getNullable: Type
    def getNonNullable: Type
    def isSubTypeOf(tpe: Type): Boolean = tpe.isInstanceOf[this.type]
    def isImplicitlyConvertibleFrom(tpe: Type): Boolean = {
      if (this == tpe)
        return true

      implicitlyConvertibleFrom.contains(tpe)
    }

    override def toString: String = name + (if (isNullable) "?" else "")

    def getSuperTypes: Set[Type] = Set()

    def implicitlyConvertibleFrom: List[Type] = List()

    def name: String
  }

  case object TError extends Type {
    override val isNullable                  = false
    override val getNullable   : TError.type = this
    override val getNonNullable: TError.type = this
    override def isSubTypeOf(tpe: Type): Boolean = true
    override val name = Errors.ErrorName
  }

  case object TUntyped extends Type {
    override val isNullable                    = false
    override val getNullable   : TUntyped.type = this
    override val getNonNullable: TUntyped.type = this
    override def isSubTypeOf(tpe: Type): Boolean = false
    override val name = "[untyped]"
  }

  case object TUnit extends Type {
    override val isNullable                 = false
    override val getNullable   : TUnit.type = this
    override val getNonNullable: TUnit.type = this
    override val name                       = "Unit"
  }

  case object TNull extends Type {
    override val getNullable   : TNull.type = this
    override val getNonNullable: TNull.type = this
    override val isNullable                 = false
    override val name                       = "null"
    override def isSubTypeOf(tpe: Type): Boolean = tpe.isNullable
  }

  object TArray {
    def apply(tpe: Type) = new TArray(tpe)
    def apply(tpe: Type, isNullable: Boolean) = new TArray(tpe, isNullable)
    def unapply(t: TArray) = Some(t.tpe)
  }
  class TArray(val tpe: Type, override val isNullable: Boolean = false) extends Type {

    override def getNullable: TArray = if (isNullable) this else TArray(tpe, isNullable = true)
    override def getNonNullable: TArray = if (isNullable) TArray(tpe, isNullable = false) else this

    override def isSubTypeOf(otherTpe: Type): Boolean = otherTpe match {
      case TArray(arrTpe) => tpe.isSubTypeOf(arrTpe)
      case _              => false
    }

    override def isImplicitlyConvertibleFrom(otherTpe: Type): Boolean = {
      if (super.isImplicitlyConvertibleFrom(otherTpe))
        return true

      otherTpe match {
        case TArray(a) => tpe.isImplicitlyConvertibleFrom(a)
        case _         => false
      }
    }

    override def name = s"$tpe[]"
    def dimension: Int = tpe match {
      case t: TArray => 1 + t.dimension
      case _         => 1
    }

    override def equals(any: Any): Boolean = any match {
      case TArray(tpe) => this.tpe == tpe
      case _           => false
    }
    override def hashCode: Int = tpe.hashCode
  }

  object TObject {
    def apply(classSymbol: ClassSymbol) = new TObject(classSymbol)
    def apply(classSymbol: ClassSymbol, isNullable: Boolean) = new TObject(classSymbol, isNullable)
    def unapply(t: TObject) = Some(t.classSymbol)
  }
  class TObject(val classSymbol: ClassSymbol, override val isNullable: Boolean = false) extends Type {

    override def getNullable: TObject = if (isNullable) this else TObject(classSymbol, isNullable = true)
    override def getNonNullable: TObject = if (isNullable) TObject(classSymbol, isNullable = false) else this
    override def isSubTypeOf(tpe: Type): Boolean = tpe match {
      case TObject(c) =>
        if (classSymbol.name == c.name) true
        else classSymbol.parents exists {_.getType.isSubTypeOf(tpe)}
      case _          => false
    }

    override def implicitlyConvertibleFrom: List[Type] = {
      if (this != Object)
        return implicitTypes

      // Object is implicitly convertible from primitive types (boxing)
      implicitTypes ::: Primitives
    }

    def implicitTypes: List[Type] = classSymbol.implicitConstructors.map(_.argList.head.getType)

    override def getSuperTypes: Set[Type] = (this :: classSymbol.parents.flatMap(_.getType.getSuperTypes)).toSet

    override def name: String = classSymbol.name

    override def equals(any: Any): Boolean = any match {
      case TObject(c) => classSymbol.name == c.name
      case _          => false
    }
    override def hashCode: Int = classSymbol.hashCode
  }

}

