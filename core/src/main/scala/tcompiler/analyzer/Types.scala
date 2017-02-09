package tcompiler
package analyzer

import tcompiler.analyzer.Symbols._
import tcompiler.code.CodeGenerator._
import tcompiler.error.Errors
import tcompiler.imports.ClassSymbolLocator


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


  private def getMainSymbol(name: String): ClassSymbol =
    ClassSymbolLocator.findSymbol(name).getOrElse(new ClassSymbol(name, isAbstract = false))

  val IntSymbol   : ClassSymbol = getMainSymbol(Main.KoolInt)
  val LongSymbol  : ClassSymbol = getMainSymbol(Main.KoolLong)
  val FloatSymbol : ClassSymbol = getMainSymbol(Main.KoolFloat)
  val DoubleSymbol: ClassSymbol = getMainSymbol(Main.KoolDouble)
  val CharSymbol  : ClassSymbol = getMainSymbol(Main.KoolChar)
  val BoolSymbol  : ClassSymbol = getMainSymbol(Main.KoolBool)
  val ObjectSymbol: ClassSymbol = getMainSymbol(Main.JavaObject)
  val StringSymbol: ClassSymbol = getMainSymbol(Main.JavaString)

  val Int            = TInt()
  val NullableInt    = TInt(isNullable = true)
  val Long           = TLong()
  val NullableLong   = TLong(isNullable = true)
  val Float          = TFloat()
  val NullableFloat  = TFloat(isNullable = true)
  val Double         = TDouble()
  val NullableDouble = TDouble(isNullable = true)
  val Char           = TChar()
  val NullableChar   = TChar(isNullable = true)
  val Bool           = TBool()
  val NullableBool   = TBool(isNullable = true)

  val Object = TObject(ObjectSymbol)
  val String = TObject(StringSymbol)

  val Array = TArray(Object)

  val Primitives = List(Int, Long, Float, Double, Char, Bool)

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

    def byteCodeName: String
    def codes: CodeMap
    def size: Int
    def name: String
  }

  case object TError extends Type {
    override val isNullable                  = false
    override val getNullable   : TError.type = this
    override val getNonNullable: TError.type = this
    override def isSubTypeOf(tpe: Type): Boolean = true
    override val name                 = Errors.ErrorName
    override val byteCodeName: String = Errors.ErrorName
    override val codes                = EmptyCodeMap
    override val size        : Int    = 0
  }

  case object TUntyped extends Type {
    override val isNullable                    = false
    override val getNullable   : TUntyped.type = this
    override val getNonNullable: TUntyped.type = this
    override def isSubTypeOf(tpe: Type): Boolean = false
    override val name                 = "[untyped]"
    override val byteCodeName: String = "UNTYPED"
    override val codes                = EmptyCodeMap
    override val size        : Int    = 0
  }

  case object TUnit extends Type {
    override val isNullable                 = false
    override val getNullable   : TUnit.type = this
    override val getNonNullable: TUnit.type = this
    override val name                       = "Unit"
    override val byteCodeName  : String     = "V"
    override val codes                      = EmptyCodeMap
    override val size          : Int        = 0
  }

  case object TNull extends Type {
    override val getNullable   : TNull.type = this
    override val getNonNullable: TNull.type = this
    override val isNullable                 = false
    override val name                       = "null"
    override def isSubTypeOf(tpe: Type): Boolean = tpe.isNullable
    override val byteCodeName: String = s"L$JavaObject;"
    override val codes                = new ObjectCodeMap(JavaObject)
    override val size        : Int    = 1
  }

  sealed abstract class PrimitiveType(symbol: ClassSymbol, override val isNullable: Boolean = false)
    extends TObject(symbol, isNullable) {

    def javaWrapper: String
    val primitiveCodeMap: CodeMap
    def primitiveByteCodeName: String

    val koolWrapper = s"kool/lang/${name}Wrapper"
    override def byteCodeName: String = if (isNullable) s"L$koolWrapper;" else primitiveByteCodeName
    override val codes: CodeMap = if (isNullable) new ObjectCodeMap(koolWrapper) else primitiveCodeMap
  }

  case class TInt(override val isNullable: Boolean = false) extends PrimitiveType(IntSymbol, isNullable) {
    override val getNullable   : TInt  = if (isNullable) NullableInt else Int
    override val getNonNullable: TInt  = if (isNullable) Int else NullableInt
    override val name                  = "Int"
    override val primitiveByteCodeName = "I"
    override val primitiveCodeMap      = IntCodeMap
    override val size                  = 1
    override val javaWrapper           = JavaInt
  }

  case class TLong(override val isNullable: Boolean = false) extends PrimitiveType(LongSymbol, isNullable) {
    override val getNullable   : TLong = if (isNullable) NullableLong else Long
    override val getNonNullable: TLong = if (isNullable) Long else NullableLong
    override val name                  = "Long"
    override val primitiveByteCodeName = "J"
    override val primitiveCodeMap      = LongCodeMap
    override val size                  = 2
    override val javaWrapper           = JavaLong
  }

  case class TFloat(override val isNullable: Boolean = false) extends PrimitiveType(FloatSymbol, isNullable) {
    override val getNullable   : TFloat = if (isNullable) NullableFloat else Float
    override val getNonNullable: TFloat = if (isNullable) Float else NullableFloat
    override val name                   = "Float"
    override val primitiveByteCodeName  = "F"
    override val primitiveCodeMap       = FloatCodeMap
    override val size                   = 1
    override val javaWrapper            = JavaFloat
  }

  case class TDouble(override val isNullable: Boolean = false) extends PrimitiveType(DoubleSymbol, isNullable) {
    override val getNullable   : TDouble = if (isNullable) NullableDouble else Double
    override val getNonNullable: TDouble = if (isNullable) Double else NullableDouble
    override val name                    = "Double"
    override val primitiveByteCodeName   = "D"
    override val primitiveCodeMap        = DoubleCodeMap
    override val size                    = 2
    override val javaWrapper             = JavaDouble
  }

  case class TChar(override val isNullable: Boolean = false) extends PrimitiveType(CharSymbol, isNullable) {
    override val getNullable   : TChar = if (isNullable) NullableChar else Char
    override val getNonNullable: TChar = if (isNullable) Char else NullableChar
    override val name                  = "Char"
    override val primitiveByteCodeName = "C"
    override val primitiveCodeMap      = CharCodeMap
    override val size                  = 1
    override val javaWrapper           = JavaChar
  }

  case class TBool(override val isNullable: Boolean = false) extends PrimitiveType(BoolSymbol, isNullable) {
    override val getNullable   : TBool = if (isNullable) NullableBool else Bool
    override val getNonNullable: TBool = if (isNullable) Bool else NullableBool
    override val name                  = "Bool"
    override val primitiveByteCodeName = "Z"
    override val primitiveCodeMap      = BoolCodeMap
    override val size                  = 1
    override val javaWrapper           = JavaBool
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
    override def byteCodeName: String = "[" + tpe.byteCodeName
    override def codes = new ArrayCodeMap(tpe.byteCodeName)
    override val size: Int = 1
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
        if (classSymbol.name == c.name || c.name == Object.name) true
        else classSymbol.parents exists {_.getType.isSubTypeOf(tpe)}
      case _          => false
    }

    override def implicitlyConvertibleFrom: List[Type] = {
      if (this != Object)
        return implicitTypes

      // Object is implicitly convertible from primitive types (boxing)
      implicitTypes ::: Primitives
    }

    def implicitTypes: List[Type] = {
      classSymbol.implicitConstructors.map(_.argList.head.getType)
    }

    override def getSuperTypes: Set[Type] = (this :: classSymbol.parents.flatMap(_.getType.getSuperTypes)).toSet

    override def name: String = classSymbol.name
    override def byteCodeName: String = {
      val name = classSymbol.name.replaceAll("\\.", "/")
      s"L$name;"
    }

    override def equals(any: Any): Boolean = any match {
      case TObject(c) => classSymbol.name == c.name
      case _          => false
    }
    override def hashCode: Int = classSymbol.hashCode

    override def codes: CodeMap = new ObjectCodeMap(classSymbol.name)
    override val size = 1
  }

}

