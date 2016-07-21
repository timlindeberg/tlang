package tcompiler
package analyzer

import tcompiler.analyzer.Symbols._
import tcompiler.ast.Trees.Implicit
import tcompiler.imports.ClassSymbolLocator
import tcompiler.code.CodeGenerator._


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

  val ObjectSymbol = ClassSymbolLocator.findSymbol(Main.TLangObject)
                     .getOrElse(new ClassSymbol(Main.TLangObject, false, false))
  val StringSymbol = ClassSymbolLocator.findSymbol(Main.TLangString)
                     .getOrElse(new ClassSymbol(Main.TLangString, false, false))
  val Object       = TObject(ObjectSymbol)
  val String       = TObject(StringSymbol)

  val Array = TArray(Object)

  val Primitives = List[Type](Int,
                               Long,
                               Float,
                               Double,
                               Char,
                               Bool)

  sealed abstract class Type {
    val isNullable: Boolean

    def getNullable: Type
    def getNonNullable: Type
    def isSubTypeOf(tpe: Type): Boolean = tpe.isInstanceOf[this.type]
    def isImplicitlyConvertableFrom(tpe: Type): Boolean = {
      if (this == tpe)
        return true

      implicitlyConvertableFrom.contains(tpe)
    }

    def getSuperTypes: Set[Type] = Set()

    def implicitlyConvertableFrom: List[Type] = List()

    def byteCodeName: String
    val codes: CodeMap
    val size : Int
  }

  case object TError extends Type {
    override val isNullable = false
    override def getNullable = this
    override def getNonNullable = this
    override def isSubTypeOf(tpe: Type): Boolean = true
    override def toString = "[error]"
    override def byteCodeName: String = "ERROR"
    override val codes     = EmptyCodeMap
    override val size: Int = 0
  }

  case object TUntyped extends Type {
    override val isNullable = false
    override def getNullable = this
    override def getNonNullable = this
    override def isSubTypeOf(tpe: Type): Boolean = false
    override def toString = "[untyped]"
    override def byteCodeName: String = "UNTYPED"
    override val codes     = EmptyCodeMap
    override val size: Int = 0
  }

  case object TUnit extends Type {
    override val isNullable = false
    override def getNullable = this
    override def getNonNullable = this
    override def toString = "Unit"
    override def byteCodeName: String = "V"
    override val codes     = EmptyCodeMap
    override val size: Int = 0
  }

  sealed abstract class PrimitiveType extends Type {
    def javaWrapper: String
    val koolWrapper = s"kool/lang/${this}Wrapper"
  }

  case class TInt(isNullable: Boolean = false) extends PrimitiveType {
    override def getNullable = if (isNullable) this else NullableInt
    override def getNonNullable = if (isNullable) Int else this
    override def implicitlyConvertableFrom = List(Char)
    override def toString = "Int"
    override def byteCodeName: String = "I"
    override def equals(any: Any) = any.isInstanceOf[TInt]
    override val codes     = IntCodeMap
    override val size: Int = 1
    override val javaWrapper = JavaInt
  }

  case class TLong(isNullable: Boolean = false) extends PrimitiveType {
    override def getNullable = if (isNullable) this else NullableLong
    override def getNonNullable = if (isNullable) Long else this
    override def implicitlyConvertableFrom = List(Char, Int)
    override def toString = "Long"
    override def byteCodeName: String = "J"
    override def equals(any: Any) = any.isInstanceOf[TLong]
    override val codes     = LongCodeMap
    override val size: Int = 2
    override val javaWrapper = JavaLong
  }

  case class TFloat(isNullable: Boolean = false) extends PrimitiveType {
    override def getNullable = if (isNullable) this else NullableFloat
    override def getNonNullable = if (isNullable) Float else this
    override def implicitlyConvertableFrom = List(Long, Char, Int)
    override def toString = "Float"
    override def byteCodeName: String = "F"
    override def equals(any: Any) = any.isInstanceOf[TFloat]
    override val codes     = FloatCodeMap
    override val size: Int = 1
    override val javaWrapper = JavaFloat
  }

  case class TDouble(isNullable: Boolean = false) extends PrimitiveType {
    override def getNullable = if (isNullable) this else NullableDouble
    override def getNonNullable = if (isNullable) Double else this
    override def implicitlyConvertableFrom = List(Float, Long, Char, Int)
    override def toString = "Double"
    override def byteCodeName: String = "D"
    override def equals(any: Any) = any.isInstanceOf[TDouble]
    override val codes     = DoubleCodeMap
    override val size: Int = 2
    override val javaWrapper = JavaDouble
  }

  case class TChar(isNullable: Boolean = false) extends PrimitiveType {
    override def getNullable = if (isNullable) this else NullableChar
    override def getNonNullable = if (isNullable) Char else this
    override def implicitlyConvertableFrom = List(Int)
    override def toString = "Char"
    override def byteCodeName: String = "C"
    override def equals(any: Any) = any.isInstanceOf[TChar]
    override val codes     = CharCodeMap
    override val size: Int = 1
    override val javaWrapper = JavaChar
  }

  case class TBool(isNullable: Boolean = false) extends PrimitiveType {
    override def getNullable = if (isNullable) this else NullableBool
    override def getNonNullable = if (isNullable) Bool else this
    override def toString = "Bool"
    override def byteCodeName: String = "Z"
    override def equals(any: Any) = any.isInstanceOf[TBool]
    override val codes     = BoolCodeMap
    override val size: Int = 1
    override val javaWrapper = JavaBool
  }

  object TArray {
    def apply(tpe: Type) = new TArray(tpe)
    def apply(tpe: Type, isNullable: Boolean) = new TArray(tpe, isNullable)
    def unapply(t: TArray) = Some(t.tpe)
  }
  class TArray(val tpe: Type, override val isNullable: Boolean = false) extends Type {
    override def getNullable = if (isNullable) this else TArray(tpe, true)
    override def getNonNullable = if (isNullable) TArray(tpe, false) else this

    override def isSubTypeOf(otherTpe: Type): Boolean = otherTpe match {
      case TArray(arrTpe) => tpe.isSubTypeOf(arrTpe)
      case _              => false
    }

    override def isImplicitlyConvertableFrom(otherTpe: Type): Boolean = {
      if (super.isImplicitlyConvertableFrom(otherTpe))
        return true

      otherTpe match {
        case TArray(a) => tpe.isImplicitlyConvertableFrom(a)
        case _         => false
      }
    }

    override def toString = tpe.toString + "[]"
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

    override def getNullable = if (isNullable) this else TObject(classSymbol, true)
    override def getNonNullable = if (isNullable) TObject(classSymbol, false) else this
    override def isSubTypeOf(tpe: Type): Boolean = tpe match {
      case TObject(c) =>
        if (classSymbol.name == c.name || c == Object.classSymbol) true
        else classSymbol.parents exists { _.getType.isSubTypeOf(tpe) }
      case _          => false
    }

    override def implicitlyConvertableFrom: List[Type] = {
      if (this != Object)
        return implicitTypes

      // Object is implicitly convertable from primitive types (boxing)
      implicitTypes ::: Primitives
    }

    def implicitTypes = {
      val implicitConstructors = classSymbol.methods.filter(m =>
        m.name == "new" &&
          m.modifiers.contains(Implicit()) &&
          m.argList.size == 1)

      implicitConstructors.map(_.argList.head.getType)
    }

    override def getSuperTypes: Set[Type] =
      (this :: classSymbol.parents.flatMap(_.getType.getSuperTypes)).toSet

    override def toString = classSymbol.name
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
    override val isNullable     = true
    override def toString = "null"
    override def isSubTypeOf(tpe: Type): Boolean = tpe.isNullable
    override def byteCodeName: String = "Ljava/lang/Object;"
    override val codes: CodeMap = EmptyCodeMap
    override val size : Int     = 1
  }

}

