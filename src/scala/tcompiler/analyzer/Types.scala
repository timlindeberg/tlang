package tcompiler
package analyzer

import tcompiler.analyzer.Symbols._
import tcompiler.ast.Trees.Implicit
import tcompiler.code.CodeGenerator._
import tcompiler.imports.ClassSymbolLocator
import tcompiler.utils.Errors


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

  val Char           = TChar()
  val NullableChar   = TChar(true)
  val Int            = TInt()
  val NullableInt    = TInt(true)
  val Long           = TLong()
  val NullableLong   = TLong(true)
  val Float          = TFloat()
  val NullableFloat  = TFloat(true)
  val Double         = TDouble()
  val NullableDouble = TDouble(true)
  val Bool           = TBool()
  val NullableBool   = TBool(true)

  val ObjectSymbol = ClassSymbolLocator.findSymbol(Main.JavaObject).get
  val StringSymbol = ClassSymbolLocator.findSymbol(Main.JavaString).get
  val Object       = TObject(ObjectSymbol)
  val String       = TObject(StringSymbol)

  val Array = TArray(Object)

  val Primitives = List(Int, Long, Float, Double, Char, Bool)

  sealed abstract class Type {
    val isNullable: Boolean

    def getNullable: Type
    def getNonNullable: Type
    def isSubTypeOf(tpe: Type): Boolean = tpe.isInstanceOf[this.type]
    def isImplicitlyConvertibleFrom(tpe: Type): Boolean = {
      if (this == tpe)
        return true

      implicitlyConvertibleFrom.contains(tpe)
    }

    override def toString = name + (if (isNullable) "?" else "")

    def getSuperTypes: Set[Type] = Set()

    def implicitlyConvertibleFrom: List[Type] = List()

    def byteCodeName: String
    def codes: CodeMap
    val size: Int
    def name: String
  }

  case object TError extends Type {
    override val isNullable = false
    override def getNullable = this
    override def getNonNullable = this
    override def isSubTypeOf(tpe: Type): Boolean = true
    override def name = Errors.ErrorName
    override def byteCodeName: String = Errors.ErrorName
    override val codes     = EmptyCodeMap
    override val size: Int = 0
  }

  case object TUntyped extends Type {
    override val isNullable = false
    override def getNullable = this
    override def getNonNullable = this
    override def isSubTypeOf(tpe: Type): Boolean = false
    override def name = "[untyped]"
    override def byteCodeName: String = "UNTYPED"
    override val codes     = EmptyCodeMap
    override val size: Int = 0
  }

  case object TUnit extends Type {
    override val isNullable = false
    override def getNullable = this
    override def getNonNullable = this
    override def name = "Unit"
    override def byteCodeName: String = "V"
    override val codes     = EmptyCodeMap
    override val size: Int = 0
  }

  sealed abstract class PrimitiveType extends Type {
    def javaWrapper: String
    val koolWrapper = s"kool/lang/${name}Wrapper"
    val primitiveCodeMap     : CodeMap
    val primitiveByteCodeName: String

    override def byteCodeName = if (isNullable) s"L$koolWrapper;" else primitiveByteCodeName
    override def codes = if (isNullable) new ObjectCodeMap(koolWrapper) else primitiveCodeMap
  }

  case class TInt(isNullable: Boolean = false) extends PrimitiveType {
    override def getNullable = if (isNullable) this else NullableInt
    override def getNonNullable = if (isNullable) Int else this
    override def implicitlyConvertibleFrom = List(Char)
    override def name = "Int"
    override def equals(any: Any) = any.isInstanceOf[TInt]
    override val primitiveByteCodeName = "I"
    override val primitiveCodeMap      = IntCodeMap
    override val size: Int             = 1
    override val javaWrapper           = JavaInt
  }

  case class TLong(isNullable: Boolean = false) extends PrimitiveType {
    override def getNullable = if (isNullable) this else NullableLong
    override def getNonNullable = if (isNullable) Long else this
    override def implicitlyConvertibleFrom = List(Char, Int)
    override def name = "Long"
    override def equals(any: Any) = any.isInstanceOf[TLong]
    override val primitiveByteCodeName = "J"
    override val primitiveCodeMap      = LongCodeMap
    override val size: Int             = 2
    override val javaWrapper           = JavaLong
  }

  case class TFloat(isNullable: Boolean = false) extends PrimitiveType {
    override def getNullable = if (isNullable) this else NullableFloat
    override def getNonNullable = if (isNullable) Float else this
    override def implicitlyConvertibleFrom = List(Long, Char, Int)
    override def name = "Float"
    override def equals(any: Any) = any.isInstanceOf[TFloat]
    override val primitiveByteCodeName = "F"
    override val primitiveCodeMap      = FloatCodeMap
    override val size: Int             = 1
    override val javaWrapper           = JavaFloat
  }

  case class TDouble(isNullable: Boolean = false) extends PrimitiveType {
    override def getNullable = if (isNullable) this else NullableDouble
    override def getNonNullable = if (isNullable) Double else this
    override def implicitlyConvertibleFrom = List(Float, Long, Char, Int)
    override def name = "Double"
    override def equals(any: Any) = any.isInstanceOf[TDouble]
    override val primitiveByteCodeName = "D"
    override val primitiveCodeMap      = DoubleCodeMap
    override val size: Int             = 2
    override val javaWrapper           = JavaDouble
  }

  case class TChar(isNullable: Boolean = false) extends PrimitiveType {
    override def getNullable = if (isNullable) this else NullableChar
    override def getNonNullable = if (isNullable) Char else this
    override def implicitlyConvertibleFrom = List(Int)
    override def name = "Char"
    override def equals(any: Any) = any.isInstanceOf[TChar]
    override val primitiveByteCodeName = "C"
    override val primitiveCodeMap      = CharCodeMap
    override val size: Int             = 1
    override val javaWrapper           = JavaChar
  }

  case class TBool(isNullable: Boolean = false) extends PrimitiveType {
    override def getNullable = if (isNullable) this else NullableBool
    override def getNonNullable = if (isNullable) Bool else this
    override def name = "Bool"
    override def equals(any: Any) = any.isInstanceOf[TBool]
    override def isImplicitlyConvertibleFrom(tpe: Type): Boolean = {
      if(super.isImplicitlyConvertibleFrom(tpe))
        return true

      // All nullable types can implicitly be converted to bool
      tpe.isNullable
    }
    override val primitiveByteCodeName = "Z"
    override val primitiveCodeMap      = BoolCodeMap
    override val size: Int             = 1
    override val javaWrapper           = JavaBool
  }

  object TArray {
    def apply(tpe: Type) = new TArray(tpe)
    def apply(tpe: Type, isNullable: Boolean) = new TArray(tpe, isNullable)
    def unapply(t: TArray) = Some(t.tpe)
  }
  class TArray(val tpe: Type, override val isNullable: Boolean = false) extends Type {
    override def getNullable = if (isNullable) this else TArray(tpe, isNullable = true)
    override def getNonNullable = if (isNullable) TArray(tpe, isNullable = false) else this

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
    override val codes     = new ArrayCodeMap(tpe.byteCodeName)
    override val size: Int = 1
    def dimension: Int = tpe match {
      case t: TArray => 1 + t.dimension
      case _         => 1
    }

    override def equals(any: Any) = any match {
      case TArray(tpe) => this.tpe == tpe
      case _           => false
    }
    override def hashCode = tpe.hashCode
  }

  object TObject {
    def apply(classSymbol: ClassSymbol) = new TObject(classSymbol)
    def apply(classSymbol: ClassSymbol, isNullable: Boolean) = new TObject(classSymbol, isNullable)
    def unapply(t: TObject) = Some(t.classSymbol)
  }
  class TObject(val classSymbol: ClassSymbol, override val isNullable: Boolean = false) extends Type {

    override def getNullable = if (isNullable) this else TObject(classSymbol, isNullable = true)
    override def getNonNullable = if (isNullable) TObject(classSymbol, isNullable = false) else this
    override def isSubTypeOf(tpe: Type): Boolean = tpe match {
      case TObject(c) =>
        if (classSymbol.name == c.name || c.name == Object.name) true
        else classSymbol.parents exists {
          _.getType.isSubTypeOf(tpe)
        }
      case _          => false
    }

    override def implicitlyConvertibleFrom: List[Type] = {
      if (this != Object)
        return implicitTypes

      // Object is implicitly convertible from primitive types (boxing)
      implicitTypes ::: Primitives
    }

    def implicitTypes = {
      val implicitConstructors = classSymbol.methods.filter(m =>
        m.name == "new" &&
          m.modifiers.contains(Implicit()) &&
          m.argList.size == 1)

      implicitConstructors.map(_.argList.head.getType)
    }

    override def getSuperTypes: Set[Type] = (this :: classSymbol.parents.flatMap(_.getType.getSuperTypes)).toSet

    override def name = classSymbol.name
    override def byteCodeName: String = {
      val name = classSymbol.name.replaceAll("\\.", "/")
      s"L$name;"
    }

    override def equals(any: Any) = any match {
      case TObject(c) => classSymbol.name == c.name
      case _          => false
    }
    override def hashCode = classSymbol.hashCode

    override val codes = new ObjectCodeMap(classSymbol.name)
    override val size  = 1
  }

  case object TNull extends Type {
    override val getNullable    = this
    override val getNonNullable = this
    override val isNullable     = false
    override def name = "null"
    override def isSubTypeOf(tpe: Type): Boolean = tpe.isNullable
    override def byteCodeName: String = s"L$JavaObject;"
    override val codes     = new ObjectCodeMap(JavaObject)
    override val size: Int = 1
  }

}

