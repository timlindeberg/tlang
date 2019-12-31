package tlang
package compiler
package imports

import org.apache.bcel.classfile._
import org.apache.bcel.generic.{
  BasicType, ObjectType, Type => BcelType, ArrayType => BcelArrayType
}
import tlang.Constants._
import tlang.compiler.analyzer.Symbols._
import tlang.compiler.analyzer.Types
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
    locateSymbol(ExtensionDecl.nameToExtensionName(className)) { clazz =>
      findExtendedClassSymbol(clazz) map { extendedClass =>
        new ExtensionClassSymbol(className) use { _.setExtendedType(Types.TObject(extendedClass)) }
      }
    }.asInstanceOf[Option[ExtensionClassSymbol]]
  }

  def fillClassSymbol(classSymbol: ClassSymbol): ClassSymbol = {
    val name = classSymbol.name
    findClass(name) ifDefined { clazz =>
      if (!SymbolCache.contains(name))
        SymbolCache += name -> classSymbol
      fillClassSymbol(classSymbol, clazz)
    }
    classSymbol
  }

  def findClass(name: String): Option[JavaClass] = classPath(name) collect {
    case c: JavaClassFile => c.parse
  }

  def classExists(name: String): Boolean = classPath(name).exists { _.isInstanceOf[JavaClassFile] }

  private def findExtendedClassSymbol(extensionClass: JavaClass): Option[ClassSymbol] = {
    extensionClass
      .getAnnotationEntries
      .find { entry => jvmObjectNameToTName(entry.getAnnotationType) == TExtensionClassAnnotation }
      .flatMap { entry =>
        entry.getElementValuePairs
          .find { pair => pair.getNameString == TExtendedClassName }
          .flatMap { pair => findSymbol(pair.getValue.stringifyValue) }
      }
  }

  private def locateSymbol(className: String)(cons: JavaClass => Option[ClassSymbol]): Option[ClassSymbol] = {
    SymbolCache.getOrElseMaybeUpdate(className) {
      findClass(className)
        .flatMap { clazz =>
          cons(clazz).map { sym => fillClassSymbol(sym, clazz) }
        }
    }
  }

  private def fillClassSymbol(classSymbol: ClassSymbol, clazz: JavaClass): ClassSymbol = {
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
    classSymbol
  }

  private def convertParents(clazz: JavaClass): List[ClassSymbol] = {
    val className = toTName(clazz.getClassName)
    // Primitives and Object have no parents
    if (className in (JavaObject :: Constants.Primitives))
      return Nil

    val parent = clazz.getSuperClass match {
      case null   => List(Types.ObjectSymbol)
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
        val isNullable = isAnnotatedWith(field, TNullableAnnotation)
        sym.setType(convertType(field.getType, isNullable))
        convertAnnotations(field.getAnnotationEntries) foreach sym.addAnnotation
        sym
      }
      .toList
  }

  private def convertMethod(meth: Method, clazz: JavaClass, owningClass: ClassSymbol): MethodSymbol = {
    var args = meth
      .getArgumentTypes
      .zip(getParameterAnnotations(meth))
      .zipWithIndex
      .map { case ((tpe, annotations), index) => convertArgument(tpe, annotations, s"arg$index") }
      .toList

    var modifiers = convertModifiers(meth)

    if (isAnnotatedWith(meth, TExtensionMethodAnnotation)) {
      modifiers -= Static() // Remove the static modifier which is added to methods in extension classes
      args = args.drop(1) // Remove the added this argument
    }

    if (isAnnotatedWith(meth, TImplicitConstructorAnnotation)) {
      modifiers += Implicit()
    }

    val name = meth.getName match {
      case "<init>" => "new"
      case name     => name
    }

    val symbol = getOperatorType(name) match {
      case Some(operatorType) => new OperatorSymbol(operatorType, owningClass, None, modifiers)
      case None               => new MethodSymbol(name, owningClass, None, modifiers)
    }

    val isNullable = isAnnotatedWith(meth, TNullableAnnotation)
    symbol.setType(convertType(meth.getReturnType, isNullable))
    symbol.argList = args
    symbol.isAbstract = meth.isAbstract

    convertAnnotations(meth.getAnnotationEntries) foreach symbol.addAnnotation

    symbol
  }

  private def convertArgument(tpe: BcelType, annotations: Array[AnnotationEntry], newName: String) = {
    val isNullable = containsAnnotation(annotations, Constants.TNullableAnnotation)
    val modifiers = Set[Modifier](Private(), Final())
    val variableSymbol = new VariableSymbol(newName, modifiers).setType(convertType(tpe, isNullable))
    convertAnnotations(annotations) foreach variableSymbol.addAnnotation
    variableSymbol
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

  private def convertType(tpe: BcelType, isNullable: Boolean): Types.Type = {
    val convertedType = tpe match {
      case x: BasicType     => x match {
        case BcelType.BOOLEAN => Types.Bool
        case BcelType.INT     => Types.Int
        case BcelType.BYTE    => Types.Int // TODO: Add byte type
        case BcelType.SHORT   => Types.Int // TODO: Add short type
        case BcelType.CHAR    => Types.Char
        case BcelType.LONG    => Types.Long
        case BcelType.FLOAT   => Types.Float
        case BcelType.DOUBLE  => Types.Double
        case BcelType.VOID    => Types.TUnit
      }
      case x: ObjectType    =>
        val name = toTName(x.getClassName)
        name match {
          case TIntRef    => Types.Int
          case TLongRef   => Types.Long
          case TFloatRef  => Types.Float
          case TDoubleRef => Types.Double
          case TCharRef   => Types.Char
          case TBoolRef   => Types.Bool
          case _          => Types.TObject(lazySymbol(name))
        }
      case x: BcelArrayType => Types.TArray(convertType(x.getBasicType, isNullable))
    }
    if (isNullable) convertedType.getNullable else convertedType
  }

  // TODO: This treats all annotation values as strings
  private def convertAnnotations(annotations: Array[AnnotationEntry]): List[AnnotationSymbol] = {
    annotations
      .map { annotation =>
        val values = annotation.getElementValuePairs
          .map { pair => (pair.getNameString, StringAnnotationValue(pair.getValue.stringifyValue)) }
          .toList
        AnnotationSymbol(jvmObjectNameToTName(annotation.getAnnotationType), values)
      }
      .toList
  }

  private def isAnnotatedWith(fieldOrMethod: FieldOrMethod, annotation: String): Boolean = {
    containsAnnotation(fieldOrMethod.getAnnotationEntries, annotation)
  }

  private def containsAnnotation(annotations: Array[AnnotationEntry], annotationName: String): Boolean = {
    annotations.exists { annotation =>
      annotationName == jvmObjectNameToTName(annotation.getAnnotationType)
    }
  }

  private def getParameterAnnotations(meth: Method) = {
    val argumentTypes = meth.getArgumentTypes
    meth
      .getParameterAnnotationEntries
      .map { _.getAnnotationEntries }
      .padTo(argumentTypes.size, Array.ofDim(0))
  }

  private def jvmObjectNameToTName(name: String) = toTName(name.drop(1).dropRight(1))

  private def getOperatorType(name: String): Option[OperatorTree] = {
    if (name.head == '$') OperatorTypes.get(name.drop(1)) else None
  }

  private def lazySymbol(name: String): ClassSymbol = {
    SymbolCache.getOrElseUpdate(name, new LazyClassSymbol(this, name))
  }

}
