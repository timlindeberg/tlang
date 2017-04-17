package tlang.compiler.imports

import org.apache.bcel.classfile._
import org.apache.bcel.generic.{BasicType, ObjectType, Type}
import tlang.Constants
import tlang.Constants._
import tlang.compiler.analyzer.Symbols._
import tlang.compiler.analyzer.Types._
import tlang.compiler.ast.Trees._
import tlang.utils.Extensions._

import scala.collection.mutable

object ClassSymbolLocator {

  // Used to make sure we never parse the same class file twice
  private val symbolCache: mutable.Map[String, ClassSymbol] = mutable.Map()

}

case class ClassSymbolLocator(classPath: ClassPath) {

  import ClassSymbolLocator._
  import ImportUtils._

  def findSymbol(className: String): Option[ClassSymbol] = {
    symbolCache.getOrElseMaybeUpdate(className) {
      _findSymbol(className) { clazz =>
        new ClassSymbol(toTName(clazz.getClassName)) use { _.isAbstract = clazz.isInterface }
      }
    }
  }

  def findExtensionSymbol(className: String): Option[ExtensionClassSymbol] = {
    symbolCache.getOrElseMaybeUpdate(className) {
      _findSymbol(className) { clazz =>
        val extensionName = toTName(clazz.getClassName)
        val originalClassName = toTName(ExtensionDecl.stripExtension(extensionName))
        val originalSymbol = findSymbol(originalClassName)

        new ExtensionClassSymbol(extensionName) use { _.setExtendedType(TObject(originalSymbol.get)) }
      }
    }.asInstanceOf[Option[ExtensionClassSymbol]]
  }

  def fillClassSymbol(classSymbol: ClassSymbol): Unit = {
    val name = classSymbol.name
    val clazz = findClass(name).get // It's an error if the class doesnt exist

    if (!symbolCache.contains(name))
      symbolCache += name -> classSymbol
    fillClassSymbol(classSymbol, clazz)
  }

  def findClass(name: String): Option[JavaClass] = classPath(name) collect {
    case c: JavaClassFile => c.parse
  }

  def classExists(name: String): Boolean = classPath(name).exists(_.isInstanceOf[JavaClassFile])

  private def _findSymbol[T <: ClassSymbol](className: String)(cons: JavaClass => T): Option[T] =
    findClass(className) map { cons(_) use { fillClassSymbol } }

  private def fillClassSymbol(classSymbol: ClassSymbol, clazz: JavaClass): Unit = {
    classSymbol.isAbstract = clazz.isInterface
    val (operators, methods) = clazz.getMethods
      .map(convertMethod(_, clazz, classSymbol))
      .toList
      .partitionInstance[OperatorSymbol]

    methods foreach classSymbol.addMethod
    operators foreach classSymbol.addOperator
    convertParents(clazz) foreach classSymbol.addParent
    convertFields(classSymbol, clazz) foreach classSymbol.addField
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
    val traits = clazz.getInterfaces.map { interface =>
      lazySymbol(toTName(interface.getClassName))
    }.toList
    parent ::: traits
  }

  private def convertFields(owningClass: ClassSymbol, clazz: JavaClass): List[FieldSymbol] = {
    clazz.getFields.map { field =>
      new FieldSymbol(field.getName, convertModifiers(field), owningClass) use {
        _.setType(convertType(field.getType))
      }
    }.toList
  }

  private def convertMethod(meth: Method, clazz: JavaClass, owningClass: ClassSymbol): MethodSymbol = {
    def isAnnotatedWith(annotation: String) = {
      val annotations = meth.getAnnotationEntries map { _.getAnnotationType.replaceAll("/", "::") }
      annotation in annotations
    }

    var modifiers = convertModifiers(meth)
    if (isAnnotatedWith(ImplicitConstructorAnnotation))
      modifiers += Implicit()

    if (isAnnotatedWith(ExtensionAnnotation))
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

    var args = meth.getArgumentTypes.zipWithIndex.map { case (tpe, i) =>
      convertArgument(tpe, s"arg$i")
    }.toList

    if (isAnnotatedWith(ExtensionAnnotation))
      args = args.drop(1) // Remove the added this argument

    symbol.argList = args
    symbol.isAbstract = meth.isAbstract

    symbol
  }

  private def convertArgument(tpe: Type, newName: String) = {
    val modifiers: Set[Modifier] = Set(Private(), Final())
    new VariableSymbol(newName, modifiers).setType(convertType(tpe))
  }

  private def convertModifiers(obj: AccessFlags): Set[Modifier] = {
    var set: Set[Modifier] = Set()
    obj match {
      case x if x.isPublic    => set += Public()
      case x if x.isProtected => set += Protected()
      case _                  => set += Private()
    }

    if (obj.isStatic) set += Static()
    if (obj.isFinal) set += Final()
    set
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

  private def getOperatorType(name: String) = {
    val e = Empty()
    name.drop(1) match {
      case "Plus"              => Plus(e, e)
      case "Minus"             => Minus(e, e)
      case "Times"             => Times(e, e)
      case "Div"               => Div(e, e)
      case "Modulo"            => Modulo(e, e)
      case "LogicAnd"          => LogicAnd(e, e)
      case "LogicOr"           => LogicOr(e, e)
      case "LogicXor"          => LogicXor(e, e)
      case "LeftShift"         => LeftShift(e, e)
      case "RightShift"        => RightShift(e, e)
      case "LessThan"          => LessThan(e, e)
      case "LessThanEquals"    => LessThanEquals(e, e)
      case "GreaterThan"       => GreaterThan(e, e)
      case "GreaterThanEquals" => GreaterThanEquals(e, e)
      case "Equals"            => Equals(e, e)
      case "NotEquals"         => NotEquals(e, e)
      case "LogicNot"          => LogicNot(e)
      case "Not"               => Not(e)
      case "Hash"              => Hash(e)
      case "PreIncrement"      => PreIncrement(e)
      case "PreDecrement"      => PreDecrement(e)
      case "ArrayRead"         => ArrayRead(e, e)
      case "Assign"            => Assign(ArrayRead(e, e), e)
      case "ArraySlice"        => ArraySlice(e, None, None, None)
      case "Negation"          => Negation(e)
      case _                   => ???
    }
  }

  private def lazySymbol(name: String) = {
    symbolCache.getOrElseUpdate(name, new LazyClassSymbol(name))
  }

  private class LazyClassSymbol(override val name: String)
    extends ClassSymbol(name) {

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
