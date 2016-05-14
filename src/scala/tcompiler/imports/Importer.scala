package tcompiler.imports

import org.apache.bcel.Repository
import org.apache.bcel.classfile._
import org.apache.bcel.generic.{BasicType, ObjectType, Type}
import tcompiler.ast.Trees.{ArrayType, _}
import tcompiler.utils.Context

import scala.collection.mutable

/**
  * Created by Tim Lindeberg on 5/14/2016.
  */

class Importer(ctx: Context, prog: Program) {

  val usedImports = mutable.Set[Import]()

  val regImports = prog.imports.filter(_.isInstanceOf[RegularImport]).asInstanceOf[List[RegularImport]]
  val wcImports  = prog.imports.filter(_.isInstanceOf[WildCardImport]).asInstanceOf[List[WildCardImport]]

  val secondClassImports = mutable.Set[String]()

  def importClass(className: String): Option[String] = {
    if (className == "")
      return None

    if (addImport(className))
      return Some(className)

    for (imp <- regImports) {
      if (imp.identifiers.last.value == className) {
        val fullName = imp.identifiers.map(_.value).mkString("/")
        if (addImport(fullName)) {
          usedImports += imp
          return Some(fullName)
        }
      }
    }

    for (imp <- wcImports) {
      val fullName = imp.identifiers.map(_.value).mkString("/") + "/" + className
      if (addImport(fullName)) {
        usedImports += imp
        return Some(fullName)
      }
    }
    None
  }

  private def addImport(className: String): Boolean = {
    getClass(className) match {
      case Some(clazz) =>
        prog.classes = findClass(clazz) :: prog.classes
        true
      case None        => false
    }
  }

  private def getClass(name: String): Option[JavaClass] =
    try {
      Some(Repository.lookupClass(name))
    } catch {
      case _: ClassNotFoundException   => None
      case e: IllegalArgumentException => None
      case e: ClassFormatException => println(e.getMessage); None
    }

  private def findClass(clazz: JavaClass): ClassDecl = {
    val name = clazz.getClassName
    val id = ClassIdentifier(name, List())
    val parents = convertParents(clazz)
    val fields = clazz.getFields.map(convertField).toList
    val methods = clazz.getMethods.map(convertMethod(_, clazz)).toList
    // TODO: Handle importing interfaces
    ExternalClassDecl(id, parents, fields, methods)
  }

  private def convertParents(clazz: JavaClass) = {
    // TODO: Handle importing interfaces

    clazz.getSuperClass match {
      case null                                                => List()
      case parent                                              =>
        prog.classes = findClass(parent) :: prog.classes
        List(ClassIdentifier(parent.getClassName, List()))
    }
  }

  private def convertField(field: Field) =
    new VarDecl(Some(convertType(field.getType)), Identifier(field.getName), None, convertModifiers(field))

  private def convertMethod(meth: Method, clazz: JavaClass): MethodDecl = {
    val name = meth.getName match {
      case "<init>" => "new"
      case n        => n
    }
    val id = Identifier(name)
    val retType = convertType(meth.getReturnType)
    val args = meth.getArgumentTypes.zipWithIndex.map { case (tpe, i) => Formal(convertType(tpe), Identifier("arg" + i)) }.toList
    val modifiers = convertModifiers(meth)

    MethodDecl(Some(retType), id, args, Some(Block(List())), modifiers)
  }

  private def convertModifiers(obj: AccessFlags): Set[Modifier] = {
    var set: Set[Modifier] = Set()
    obj match {
      case x if x.isPublic    => set += Public()
      case x if x.isProtected => set += Protected()
      case _                  => set += Private()
    }

    if (obj.isStatic) set += Static()
    set
  }

  private def convertType(tpe: Type): TypeTree = tpe match {
    case x: BasicType                         => x match {
      case Type.BOOLEAN => BooleanType()
      case Type.INT     => IntType()
      case Type.BYTE    => IntType() // TODO: Add byte type
      case Type.SHORT   => IntType() // TODO: Add short type
      case Type.CHAR    => CharType()
      case Type.LONG    => LongType()
      case Type.FLOAT   => FloatType()
      case Type.DOUBLE  => DoubleType()
      case Type.VOID    => UnitType()
      case _            => IntType()
    }
    case x: ObjectType                        =>
      secondClassImports += x.getClassName
      ClassIdentifier(x.getClassName)
    case x: org.apache.bcel.generic.ArrayType => ArrayType(convertType(x.getBasicType))
  }
}