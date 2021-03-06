package tlang
package compiler
package analyzer

import tlang.Constants._
import tlang.compiler.analyzer.Symbols._
import tlang.compiler.imports.{ClassPath, ClassSymbolLocator}
import tlang.compiler.messages.CompilerMessage

object Types {

  object Typed {
    def unapply(arg: Typed): Option[Type] = Some(arg.getType)
  }
  trait Typed {
    self =>

    protected var _tpe: Type = TUntyped

    def setType(tpe: Typed): self.type = setType(tpe.getType)
    def setType(tpe: Type): self.type = {
      _tpe = tpe
      this
    }

    def getType: Type = _tpe
    def hasType: Boolean = getType != TUntyped
  }

  val IntSymbol = new ClassSymbol(TInt)
  val LongSymbol = new ClassSymbol(TLong)
  val FloatSymbol = new ClassSymbol(TFloat)
  val DoubleSymbol = new ClassSymbol(TDouble)
  val CharSymbol = new ClassSymbol(TChar)
  val BoolSymbol = new ClassSymbol(TBool)
  val ObjectSymbol = new ClassSymbol(JavaObject)
  val StringSymbol = new ClassSymbol(JavaString)
  val ExtensionMethodAnnotationSymbol = new ClassSymbol(TExtensionMethodAnnotation)
  val ExtensionClassAnnotationSymbol = new ClassSymbol(TExtensionClassAnnotation)
  val ImplicitConstructorAnnotationSymbol = new ClassSymbol(TImplicitConstructorAnnotation)

  val Int: TObject = TObject(IntSymbol)
  val Long: TObject = TObject(LongSymbol)
  val Float: TObject = TObject(FloatSymbol)
  val Double: TObject = TObject(DoubleSymbol)
  val Char: TObject = TObject(CharSymbol)
  val Bool: TObject = TObject(BoolSymbol)
  val Object: TObject = TObject(ObjectSymbol)
  val String: TObject = TObject(StringSymbol)
  val Array: TArray = TArray(Object)
  val ExtensionMethodAnnotation: TObject = TObject(ExtensionMethodAnnotationSymbol)
  val ExtensionClassAnnotation: TObject = TObject(ExtensionClassAnnotationSymbol)
  val ImplicitConstructorAnnotation: TObject = TObject(ImplicitConstructorAnnotationSymbol)

  val Primitives: List[TObject] = List(Int, Long, Float, Double, Char, Bool)
  val DefaultTypes: List[TObject] = Primitives ++ List(String, Object)

  val AnnotationTypes: List[Type] = List(Int, Long, Float, Double, String)

  private def initialize(symbols: ClassSymbol*): Unit = {
    val classSymbolLocator = ClassSymbolLocator(ClassPath.Default)
    for (sym <- symbols)
      classSymbolLocator fillClassSymbol sym
  }

  initialize(
    IntSymbol,
    LongSymbol,
    FloatSymbol,
    DoubleSymbol,
    CharSymbol,
    BoolSymbol,
    ObjectSymbol,
    StringSymbol,
    ExtensionMethodAnnotationSymbol,
    ExtensionClassAnnotationSymbol,
    ImplicitConstructorAnnotationSymbol
  )

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

    def getSuperTypes: Set[Type] = Set(Object)

    def implicitlyConvertibleFrom: List[Type] = List()

    def name: String
  }

  case object TError extends Type {
    override val isNullable: Boolean = false
    override val getNullable: TError.type = this
    override val getNonNullable: TError.type = this
    override def isSubTypeOf(tpe: Type): Boolean = true
    override val name: String = CompilerMessage.ErrorName
  }

  case object TUntyped extends Type {
    override val isNullable: Boolean = false
    override val getNullable: TUntyped.type = this
    override val getNonNullable: TUntyped.type = this
    override def isSubTypeOf(tpe: Type): Boolean = false
    override val name: String = "[untyped]"
  }

  case object TUnit extends Type {
    override val isNullable: Boolean = false
    override val getNullable: TUnit.type = this
    override val getNonNullable: TUnit.type = this
    override val name: String = "Unit"
  }

  case object TNull extends Type {
    override val getNullable: TNull.type = this
    override val getNonNullable: TNull.type = this
    override val isNullable: Boolean = false
    override val name: String = "Null"
    override def isSubTypeOf(tpe: Type): Boolean = tpe.isNullable
  }

  object TArray {
    def apply(tpe: Type) = new TArray(tpe)
    def apply(tpe: Type, isNullable: Boolean) = new TArray(tpe, isNullable)
    def unapply(t: TArray): Option[Type] = Some(t.tpe)
  }
  class TArray(val tpe: Type, override val isNullable: Boolean = false) extends Type {

    override def getNullable: TArray = if (isNullable) this else TArray(tpe, isNullable = true)
    override def getNonNullable: TArray = if (isNullable) TArray(tpe, isNullable = false) else this

    override def isSubTypeOf(otherTpe: Type): Boolean = otherTpe match {
      case Object         => true
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
    def unapply(t: TObject): Option[ClassSymbol] = Some(t.classSymbol)
  }
  class TObject(val classSymbol: ClassSymbol, override val isNullable: Boolean = false) extends Type {

    override def getNullable: TObject = if (isNullable) this else TObject(classSymbol, isNullable = true)
    override def getNonNullable: TObject = if (isNullable) TObject(classSymbol, isNullable = false) else this
    override def isSubTypeOf(tpe: Type): Boolean = tpe match {
      case TObject(c) =>
        if (classSymbol.name == c.name || c.name == JavaObject) true
        else classSymbol.parents exists { _.getType.isSubTypeOf(tpe) }
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

