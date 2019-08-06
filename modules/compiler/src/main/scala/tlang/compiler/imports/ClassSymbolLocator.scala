package tlang
package compiler
package imports

import org.apache.bcel.classfile._
import org.apache.bcel.generic.{BasicType, ObjectType, Type}
import tlang.Constants._
import tlang.compiler.analyzer.Symbols._
import tlang.compiler.analyzer.Types._
import tlang.compiler.ast.Trees._

import scala.collection.mutable

object ClassSymbolLocator {

  // Used to make sure we never parse the same class file twice
  private val SymbolCache: mutable.Map[String, ClassSymbol] = mutable.Map()

  private val OperatorTypes: Map[String, OperatorTree] = {
    val e = Empty()
    Map(
      "Plus" -> Plus(e, e),
      "Minus" -> Minus(e, e),
      "Times" -> Times(e, e),
      "Div" -> Div(e, e),
      "Modulo" -> Modulo(e, e),
      "LogicAnd" -> LogicAnd(e, e),
      "LogicOr" -> LogicOr(e, e),
      "LogicXor" -> LogicXor(e, e),
      "LeftShift" -> LeftShift(e, e),
      "RightShift" -> RightShift(e, e),
      "LessThan" -> LessThan(e, e),
      "LessThanEquals" -> LessThanEquals(e, e),
      "GreaterThan" -> GreaterThan(e, e),
      "GreaterThanEquals" -> GreaterThanEquals(e, e),
      "Equals" -> Equals(e, e),
      "NotEquals" -> NotEquals(e, e),
      "LogicNot" -> LogicNot(e),
      "Not" -> Not(e),
      "Hash" -> Hash(e),
      "PreIncrement" -> PreIncrement(e),
      "PreDecrement" -> PreDecrement(e),
      "ArrayRead" -> ArrayRead(e, e),
      "Assign" -> Assign(ArrayRead(e, e), e),
      "ArraySlice" -> ArraySlice(e, None, None, None),
      "Negation" -> Negation(e)
    )
  }
}

case class ClassSymbolLocator(classPath: ClassPath) {

  import ClassSymbolLocator._
  import ImportUtils._

  def findSymbol(className: String): Option[ClassSymbol] = {
    locateSymbol(className) { clazz =>
      Some(new ClassSymbol(toTName(clazz.getClassName)) use { _.isAbstract = clazz.isInterface })
    }
  }

  def findExtensionSymbol(className: String): Option[ExtensionClassSymbol] = {
    locateSymbol(className) { clazz =>
      val extensionName = toTName(clazz.getClassName)
      findExtendedClassSymbol(clazz) map { extendedClass =>
        new ExtensionClassSymbol(extensionName) use { _.setExtendedType(TObject(extendedClass)) }
      }
    }.asInstanceOf[Option[ExtensionClassSymbol]]
  }

  def fillClassSymbol(classSymbol: ClassSymbol): ClassSymbol = {
    val name  = classSymbol.name
    val clazz = findClass(name).get // It's an error if the class doesnt exist

    if (!SymbolCache.contains(name))
      SymbolCache += name -> classSymbol
    fillClassSymbol(classSymbol, clazz)
    classSymbol
  }

  def findClass(name: String): Option[JavaClass] = classPath(name) collect {
    case c: JavaClassFile => c.parse
  }

  def classExists(name: String): Boolean = classPath(name).exists { _.isInstanceOf[JavaClassFile] }

  private def findExtendedClassSymbol(extensionClass: JavaClass): Option[ClassSymbol] = {
    extensionClass
      .getAnnotationEntries
      .find { entry => entry.getAnnotationType == Constants.TExtensionAnnotation }
      .flatMap { entry =>
        entry.getElementValuePairs
          .find { pair => pair.getNameString == "$ExtendedClass" }
          .flatMap { pair => findSymbol(pair.getValue.stringifyValue) }
      }
  }

  private def locateSymbol(className: String)(cons: JavaClass => Option[ClassSymbol]): Option[ClassSymbol] = {
    SymbolCache.getOrElseMaybeUpdate(className) {
      findClass(className) flatMap { cons(_) map { fillClassSymbol } }
    }
  }

  private def fillClassSymbol(classSymbol: ClassSymbol, clazz: JavaClass): Unit = {
    classSymbol.isAbstract = clazz.isInterface
    val (operators, methods) = clazz.getMethods
      .map { convertMethod(_, clazz, classSymbol) }
      .toList
      .partitionInstance[OperatorSymbol]

    methods foreach classSymbol.addMethod
    operators foreach classSymbol.addOperator
    convertParents(clazz) foreach classSymbol.addParent
    convertFields(classSymbol, clazz) foreach classSymbol.addField
    convertAnnotations(clazz.getAnnotationEntries) foreach classSymbol.addAnnotation
  }

  private def convertParents(clazz: JavaClass): List[ClassSymbol] = {
    val className = toTName(clazz.getClassName)
    // Primitives and Object have no parents
    if (className in (JavaObject :: Constants.Primitives))
      return Nil

    val parent = clazz.getSuperClass match {
      case null   => List(ObjectSymbol)
      case parent => List(lazySymbol(toTName(parent.getClassName)))
    }
    val traits = clazz
      .getInterfaces
      .map { interface => lazySymbol(toTName(interface.getClassName)) }
      .toList
    parent ::: traits
  }

  private def convertFields(owningClass: ClassSymbol, clazz: JavaClass): List[FieldSymbol] = {
    clazz
      .getFields
      .map { field =>
        val sym = new FieldSymbol(field.getName, convertModifiers(field), owningClass)
        sym.setType(convertType(field.getType))
        convertAnnotations(field.getAnnotationEntries) foreach sym.addAnnotation
        sym
      }
      .toList
  }

  private def convertMethod(meth: Method, clazz: JavaClass, owningClass: ClassSymbol): MethodSymbol = {
    var modifiers   = convertModifiers(meth)
    val annotations = meth.getAnnotationEntries
    if (isAnnotatedWith(annotations, TImplicitConstructorAnnotation))
      modifiers += Implicit()

    if (isAnnotatedWith(annotations, TExtensionAnnotation))
      modifiers -= Static() // Remove the static modifier which is added to methods in extension classes

    val name = meth.getName match {
      case "<init>" => "new"
      case name     => name
    }

    val symbol = name.head match {
      case '$' =>
        val operatorType = getOperatorType(name)
        new OperatorSymbol(operatorType, owningClass, None, modifiers)
      case _   => new MethodSymbol(name, owningClass, None, modifiers)
    }

    symbol.setType(convertType(meth.getReturnType))

    var args = meth
      .getArgumentTypes
      .zipWithIndex
      .map { case (tpe, i) => convertArgument(tpe, s"arg$i") }
      .toList

    if (isAnnotatedWith(annotations, TExtensionAnnotation))
      args = args.drop(1) // Remove the added this argument

    symbol.argList = args
    symbol.isAbstract = meth.isAbstract

    convertAnnotations(meth.getAnnotationEntries) foreach symbol.addAnnotation

    symbol
  }


  private def convertArgument(tpe: Type, newName: String) = {
    val modifiers: Set[Modifier] = Set(Private(), Final())
    new VariableSymbol(newName, modifiers).setType(convertType(tpe))
  }

  private def convertModifiers(obj: AccessFlags): Set[Modifier] = {
    val set = mutable.Set[Modifier]()
    obj match {
      case x if x.isPublic    => set += Public()
      case x if x.isProtected => set += Protected()
      case _                  => set += Private()
    }

    if (obj.isStatic) set += Static()
    if (obj.isFinal) set += Final()
    set.toSet
  }

  private def convertType(tpe: Type): tlang.compiler.analyzer.Types.Type = tpe match {
    case x: BasicType                         => x match {
      case Type.BOOLEAN => Bool
      case Type.INT     => Int
      case Type.BYTE    => Int // TODO: Add byte type
      case Type.SHORT   => Int // TODO: Add short type
      case Type.CHAR    => Char
      case Type.LONG    => Long
      case Type.FLOAT   => Float
      case Type.DOUBLE  => Double
      case Type.VOID    => TUnit
    }
    case x: ObjectType                        =>
      val name = toTName(x.getClassName)
      name match {
        case TIntRef    => Int.getNullable
        case TLongRef   => Long.getNullable
        case TFloatRef  => Float.getNullable
        case TDoubleRef => Double.getNullable
        case TCharRef   => Char.getNullable
        case TBoolRef   => Bool.getNullable
        case _          => TObject(lazySymbol(name))
      }
    case x: org.apache.bcel.generic.ArrayType => TArray(convertType(x.getBasicType))
  }

  // TODO: This treats all annotation values as strings
  private def convertAnnotations(annotations: Array[AnnotationEntry]): List[AnnotationSymbol] = {
    annotations
      .map { annotation =>
        val values = annotation.getElementValuePairs
          .map { pair => (pair.getNameString, StringAnnotationValue(pair.getValue.stringifyValue)) }
          .toList
        AnnotationSymbol(toTName(annotation.getAnnotationType), values)
      }
      .toList
  }

  private def isAnnotatedWith(annotations: Array[AnnotationEntry], annotation: String): Boolean = {
    annotation in annotations.map { _.getAnnotationType.replaceAll("/", "::") }
  }

  private def getOperatorType(name: String) = OperatorTypes(name.drop(1))

  private def lazySymbol(name: String) = {
    SymbolCache.getOrElseUpdate(name, new LazyClassSymbol(name))
  }

  private class LazyClassSymbol(override val name: String) extends ClassSymbol(name) {

    var loaded = false

    override def methods: List[MethodSymbol] = {
      loadClassSymbol()
      _methods
    }

    override def operators: List[OperatorSymbol] = {
      loadClassSymbol()
      _operators
    }

    override def fields: Map[String, FieldSymbol] = {
      loadClassSymbol()
      _fields
    }

    override def isAbstract: Boolean = {
      loadClassSymbol()
      _isAbstract
    }

    private def loadClassSymbol(): Unit =
      if (!loaded) {
        fillClassSymbol(this)
        loaded = true
      }
  }


}
