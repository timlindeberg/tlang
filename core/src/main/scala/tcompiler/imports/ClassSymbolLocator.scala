package tcompiler.imports

import org.apache.bcel.Repository
import org.apache.bcel.classfile._
import org.apache.bcel.generic.{BasicType, ObjectType, Type}
import org.apache.bcel.util.{ClassPath, SyntheticRepository}
import tcompiler.Main
import tcompiler.analyzer.Symbols._
import tcompiler.analyzer.Types._
import tcompiler.ast.Trees._
import tcompiler.utils.Extensions._

/**
  * Created by Tim Lindeberg on 5/14/2016.
  */
object ClassSymbolLocator {

  def setClassPath(classPaths: List[String]): Unit = {
    val rep = SyntheticRepository.getInstance(new ClassPath(classPaths.mkString(";")))
    Repository.setRepository(rep)
  }

  def findSymbol(className: String): Option[ClassSymbol] =
    _findSymbol(className, clazz => new ClassSymbol(clazz.getClassName, clazz.isInterface))

  def findExtensionSymbol(className: String): Option[ExtensionClassSymbol] =
    _findSymbol(className, clazz => {
      val extensionName = clazz.getClassName
      val originalClassName = extensionName.replaceAll(".*\\$EX\\.", "")
      val originalSymbol = findSymbol(originalClassName)

      new ExtensionClassSymbol(extensionName).use(_.originalClassSymbol = originalSymbol)
    })

  private def _findSymbol[T <: ClassSymbol](className: String, cons: JavaClass => T): Option[T] = {
    findClass(className) match {
      case Some(javaClass) =>
        val classSymbol = cons(javaClass)
        fillClassSymbol(classSymbol)
        Some(classSymbol)
      case None            => None
    }
  }

  def fillClassSymbol(classSymbol: ClassSymbol): Unit = {
    findClass(classSymbol.name) match {
      case Some(javaClass) => fillClassSymbol(classSymbol, javaClass)
      case None            =>
    }
  }

  def findClass(name: String): Option[JavaClass] =
    try {
      Some(Repository.lookupClass(name))
    } catch {
      case _: ClassNotFoundException   => None
      case _: IllegalArgumentException => None
      case _: ClassFormatException     => None
    }

  def classExists(name: String): Boolean = findClass(name).isDefined

  def clearCache(): Unit = Repository.clearCache()

  private def fillClassSymbol(classSymbol: ClassSymbol, clazz: JavaClass): Unit = {
    val methods = clazz.getMethods.map(convertMethod(_, clazz, classSymbol)).toList
    classSymbol.methods = methods.filterNotInstance[OperatorSymbol]
    classSymbol.operators = methods.filterInstance[OperatorSymbol]
    classSymbol.parents = convertParents(clazz)
    classSymbol.isAbstract = clazz.isAbstract
    classSymbol.fields = clazz.getFields.map { field =>
      val f = convertField(classSymbol, field)
      (f.name, f)
    }.toMap
  }

  private def convertParents(clazz: JavaClass): List[ClassSymbol] = {
    val parent = clazz.getSuperClass match {
      case null   => List()
      case parent => List(incompleteClass(parent.getClassName, parent.isAbstract))
    }
    val traits = clazz.getInterfaces.map(interface => incompleteClass(interface.getClassName, isAbstract = true)).toList
    parent ::: traits
  }

  private def convertField(owningClass: ClassSymbol, field: Field): FieldSymbol = {
    val f = new FieldSymbol(field.getName, convertModifiers(field), owningClass)
    val tpe = convertType(field.getType)
    f.setType(tpe)
    f
  }

  private def convertMethod(meth: Method, clazz: JavaClass, owningClass: ClassSymbol): MethodSymbol = {

    var modifiers = convertModifiers(meth)

    val name = meth.getName match {
      case "<init>" => "new"
      case name     => name
    }

    val isExtensionMethod = meth.getAnnotationEntries.exists(_.getAnnotationType == Main.TExtensionAnnotation)
    if (isExtensionMethod)
      modifiers -= Static() // Remove the added static modifier

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

    if (isExtensionMethod)
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

  private def convertType(tpe: Type): tcompiler.analyzer.Types.Type = tpe match {
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
      val name = x.getClassName
      name.replaceAll("\\.", "/") match {
        case x if x == Int.koolWrapper    => NullableInt
        case x if x == Bool.koolWrapper   => NullableBool
        case x if x == Char.koolWrapper   => NullableChar
        case x if x == Double.koolWrapper => NullableDouble
        case x if x == Float.koolWrapper  => NullableFloat
        case x if x == Long.koolWrapper   => NullableLong
        case _                            => TObject(incompleteClass(x))
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
      case x                   => ???
    }
  }

  private def incompleteClass(tpe: ObjectType): ClassSymbol =
    incompleteClass(tpe.getClassName, tpe.getClass.isInterface)

  private def incompleteClass(name: String, isAbstract: Boolean): ClassSymbol =
    new ClassSymbol(name, isAbstract, isComplete = false)

}
