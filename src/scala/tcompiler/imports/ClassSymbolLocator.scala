package tcompiler.imports

import org.apache.bcel.Repository
import org.apache.bcel.classfile._
import org.apache.bcel.generic.{BasicType, ObjectType, Type}
import org.apache.bcel.util.{ClassPath, SyntheticRepository}
import tcompiler.analyzer.Symbols.{Field => _, _}
import tcompiler.analyzer.Types._
import tcompiler.ast.Trees._

/**
  * Created by Tim Lindeberg on 5/14/2016.
  */
object ClassSymbolLocator {

  def setClassPath(classPaths: List[String]) = {
    val rep = SyntheticRepository.getInstance(new ClassPath(classPaths.mkString(";")))
    Repository.setRepository(rep)
  }

  def findSymbol(className: String): Option[ClassSymbol] = {
    findClass(className) match {
      case Some(javaClass) => Some(convertClass(javaClass))
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

  def classExists(name: String) = findClass(name).isDefined

  def clearCache() = Repository.clearCache()

  private def fillClassSymbol(classSymbol: ClassSymbol, clazz: JavaClass): Unit = {
    val methods = clazz.getMethods.map(convertMethod(_, clazz, classSymbol)).toList
    classSymbol.methods = methods.filter(!_.isInstanceOf[OperatorSymbol])
    classSymbol.operators = methods.filter(_.isInstanceOf[OperatorSymbol]).asInstanceOf[List[OperatorSymbol]]
    classSymbol.parents = convertParents(clazz)
    classSymbol.fields = clazz.getFields.map { field =>
      val f = convertField(classSymbol, field)
      (f.name, f)
    }.toMap
    classSymbol.setType(TObject(classSymbol))
    classSymbol.writtenName = classSymbol.name
  }


  private def convertClass(clazz: JavaClass): ClassSymbol = {
    val name = clazz.getClassName

    val symbol = new ClassSymbol(name, clazz.isInterface)
    fillClassSymbol(symbol)
    symbol
  }

  private def convertParents(clazz: JavaClass): List[ClassSymbol] = {
    val parent = clazz.getSuperClass match {
      case null   => List()
      case parent => List(incompleteClass(parent.getClassName, parent.isAbstract))
    }
    val traits = clazz.getInterfaces.map(interface => incompleteClass(interface.getClassName, true)).toList
    parent ::: traits
  }

  private def convertField(owningClass: ClassSymbol, field: Field): VariableSymbol = {
    val f = new VariableSymbol(field.getName, tcompiler.analyzer.Symbols.Field, convertModifiers(field), Some(owningClass))
    val tpe = convertType(field.getType)
    f.setType(tpe)
    f
  }

  private def convertMethod(meth: Method, clazz: JavaClass, owningClass: ClassSymbol): MethodSymbol = {
    val modifiers = convertModifiers(meth)
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

    symbol.argList = meth.getArgumentTypes.zipWithIndex.map {
      case (tpe, i) => convertArgument(tpe, s"arg$i")
    }.toList

    symbol.isAbstract = meth.isAbstract

    symbol
  }

  private def convertArgument(tpe: Type, newName: String) = {
    val modifiers: Set[Modifier] = Set(Private(), Final())
    new VariableSymbol(newName, tcompiler.analyzer.Symbols.Argument, modifiers).setType(convertType(tpe))
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
      case Type.BOOLEAN => TBool
      case Type.INT     => TInt
      case Type.BYTE    => TInt // TODO: Add byte type
      case Type.SHORT   => TInt // TODO: Add short type
      case Type.CHAR    => TChar
      case Type.LONG    => TLong
      case Type.FLOAT   => TFloat
      case Type.DOUBLE  => TDouble
      case Type.VOID    => TUnit
    }
    case x: ObjectType                        => TObject(incompleteClass(x))
    case x: org.apache.bcel.generic.ArrayType => TArray(convertType(x.getBasicType))
  }

  private def getOperatorType(name: String) = {
    name.drop(1) match {
      case "Plus"              => Plus(Empty(), Empty())
      case "Minus"             => Minus(Empty(), Empty())
      case "Times"             => Times(Empty(), Empty())
      case "Div"               => Div(Empty(), Empty())
      case "Modulo"            => Modulo(Empty(), Empty())
      case "LogicAnd"          => LogicAnd(Empty(), Empty())
      case "LogicOr"           => LogicOr(Empty(), Empty())
      case "LogicXor"          => LogicXor(Empty(), Empty())
      case "LeftShift"         => LeftShift(Empty(), Empty())
      case "RightShift"        => RightShift(Empty(), Empty())
      case "LessThan"          => LessThan(Empty(), Empty())
      case "LessThanEquals"    => LessThanEquals(Empty(), Empty())
      case "GreaterThan"       => GreaterThan(Empty(), Empty())
      case "GreaterThanEquals" => GreaterThanEquals(Empty(), Empty())
      case "Equals"            => Equals(Empty(), Empty())
      case "NotEquals"         => NotEquals(Empty(), Empty())
      case "LogicNot"          => LogicNot(Empty())
      case "Not"               => Not(Empty())
      case "Hash"              => Hash(Empty())
      case "PreIncrement"      => PreIncrement(Empty())
      case "PreDecrement"      => PreDecrement(Empty())
      case "ArrayRead"         => ArrayRead(Empty(), Empty())
      case "ArrayAssign"       => ArrayAssign(Empty(), Empty(), Empty())
      case x                   => ???
    }
  }

  private def incompleteClass(tpe: ObjectType): ClassSymbol =
    incompleteClass(tpe.getClassName, tpe.getClass.isInterface)
  private def incompleteClass(name: String, isAbstract: Boolean): ClassSymbol =
    new ClassSymbol(name, isAbstract, isComplete = false)

}
