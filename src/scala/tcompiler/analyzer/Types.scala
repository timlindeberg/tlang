package tcompiler
package analyzer

import tcompiler.analyzer.Symbols._
import tcompiler.ast.Trees.Implicit
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

  val Char   = TChar()
  val Int    = TInt()
  val Long   = TLong()
  val Float  = TFloat()
  val Double = TDouble()
  val Bool   = TBool()

  var Object = TObject(ClassSymbolLocator.findSymbol(Main.TLangObject)
    .getOrElse(new ClassSymbol(Main.TLangObject, false)))
  var String = TObject(ClassSymbolLocator.findSymbol(Main.TLangString)
    .getOrElse(new ClassSymbol(Main.TLangString, false)))

  val Array = TArray(Object)


  val Primitives = List[Type](
    Int,
    Long,
    Float,
    Double,
    Char)

  sealed abstract class Type {
    val isNullable: Boolean

    def getNullable: Type
    def isSubTypeOf(tpe: Type): Boolean = tpe.isInstanceOf[this.type]
    def isImplicitlyConvertableFrom(tpe: Type): Boolean = {
      if (this == tpe)
        return true

      val implicitTypes = implicitlyConvertableFrom()
      if (implicitTypes.contains(tpe))
        return true


      (this, tpe) match {
        case (TArray(a1), TArray(a2)) => a1.isImplicitlyConvertableFrom(a2)
        case _                              => false
      }
    }
    def getSuperTypes: List[Type] = List()
    def isPrimitive = Primitives.contains(this)

    def implicitlyConvertableFrom(): List[Type] = List()

    def byteCodeName: String
    val codes: CodeMap
    val size : Int
  }

  case object TError extends Type {
    override val isNullable = false
    override def getNullable = this
    override def isSubTypeOf(tpe: Type): Boolean = true
    override def toString = "[error]"
    override def byteCodeName: String = "ERROR"
    override val codes     = EmptyCodeMap
    override val size: Int = 0
  }

  case object TUntyped extends Type {
    override val isNullable = false
    override def getNullable = this

    override def isSubTypeOf(tpe: Type): Boolean = false
    override def toString = "[untyped]"
    override def byteCodeName: String = "UNTYPED"
    override val codes     = EmptyCodeMap
    override val size: Int = 0
  }

  case object TUnit extends Type {
    override val isNullable = false
    override def getNullable = this

    override def toString = "Unit"
    override def byteCodeName: String = "V"
    override val codes     = EmptyCodeMap
    override val size: Int = 0
  }

  case class TInt(isNullable: Boolean = false) extends Type {
    override def getNullable = if (isNullable) this else TInt(true)
    override def implicitlyConvertableFrom() = List(Char)
    override def toString = "Int"
    override def byteCodeName: String = "I"
    override val codes     = IntCodeMap
    override val size: Int = 1
  }

  case class TLong(isNullable: Boolean = false) extends Type {
    override def getNullable = if (isNullable) this else TLong(true)
    override def implicitlyConvertableFrom() = List(Char, Int)
    override def toString = "Long"
    override def byteCodeName: String = "J"
    override val codes     = LongCodeMap
    override val size: Int = 2
  }

  case class TFloat(isNullable: Boolean = false) extends Type {
    override def getNullable = if (isNullable) this else TFloat(true)
    override def implicitlyConvertableFrom() = List(Long, Char, Int)
    override def toString = "Float"
    override def byteCodeName: String = "F"
    override val codes     = FloatCodeMap
    override val size: Int = 1
  }

  case class TDouble(isNullable: Boolean = false) extends Type {
    override def getNullable = if (isNullable) this else TDouble(true)
    override def implicitlyConvertableFrom() = List(Float, Long, Char, Int)
    override def toString = "Double"
    override def byteCodeName: String = "D"
    override val codes     = DoubleCodeMap
    override val size: Int = 2
  }

  case class TChar(isNullable: Boolean = false) extends Type {
    override def getNullable = if (isNullable) this else TChar(true)
    override def toString = "Char"
    override def byteCodeName: String = "C"
    override val codes     = CharCodeMap
    override val size: Int = 1
  }

  case class TBool(isNullable: Boolean = false) extends Type {
    override def getNullable = if (isNullable) this else TBool(true)
    override def toString = "Bool"
    override def byteCodeName: String = "Z"
    override val codes     = BoolCodeMap
    override val size: Int = 1
  }

  object TArray {
    def apply(tpe: Type) = new TArray(tpe)
    def apply(tpe: Type, isNullable: Boolean) = new TArray(tpe, isNullable)
    def unapply(t: TArray) = Some(t.tpe)
  }
  class TArray(val tpe: Type, override val isNullable: Boolean = false) extends Type {
    override def getNullable = new TArray(tpe, true)
    override def isSubTypeOf(otherTpe: Type): Boolean = otherTpe match {
      case TArray(arrTpe) => tpe.isSubTypeOf(arrTpe)
      case _              => false
    }

    override def implicitlyConvertableFrom() = List()
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
    override def isSubTypeOf(tpe: Type): Boolean = tpe match {
      case TObject(c) =>
        if (classSymbol.name == c.name || c == Object.classSymbol) true
        else classSymbol.parents exists { parent => parent.getType.isSubTypeOf(tpe) }
      case _             => false
    }

    override def implicitlyConvertableFrom() =
      classSymbol.methods.filter(m =>
        m.name == "new" &&
          m.modifiers.contains(Implicit()) &&
          m.argList.size == 1).
        map(_.argList.head.getType)

    override def getSuperTypes: List[Type] =
      this :: classSymbol.parents.flatMap(_.getType.getSuperTypes)

    override def toString = classSymbol.name
    override def byteCodeName: String = {
      val name = classSymbol.name.replaceAll("\\.", "/")
      s"L$name;"
    }

    override def equals(any: Any) = any match {
      case TObject(c) => classSymbol == c
      case _           => false
    }
    override def hashCode = classSymbol.hashCode

    override val codes = new ObjectCodeMap(classSymbol.name)
    override val size  = 1
  }

  case object TNull extends Type {
    override val getNullable = this
    override val isNullable  = true
    override def byteCodeName: String = "Ljava/lang/Object;"
    override val codes: CodeMap = EmptyCodeMap
    override val size : Int     = 1
  }

}

