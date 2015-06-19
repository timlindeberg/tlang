package koolc
package modification

import koolc.ast.{Printer, Trees}
import koolc.utils.Pipeline
import koolc.utils.Context
import koolc.ast.Trees._
import org.apache.bcel._
import org.apache.bcel.classfile.{JavaClass, Method}
import org.apache.bcel.generic.{ObjectType, BasicType, Type}
import org.apache.bcel.Constants

object Imports extends Pipeline[Program, Program] {

  def run(ctx: Context)(prog: Program): Program = {
    import ctx.reporter._

    var addedExternalClasses = Map[String, String]()
    var addedClasses = Set[String]()

    object Importer {

      def addClass(className: String): Unit = {
        (Import(List()) :: prog.imports).takeWhile { imp =>
          try{
            val importName = imp.identifiers.map(_.value).mkString("/") + "/"
            println("Trying " + importName + className)
            addClass(findClass(Repository.lookupClass(importName + className)))
            addedExternalClasses += (className -> (importName + className))
            false
          } catch {
            case _: ClassNotFoundException =>
              true
          }
        }
      }

      def addClass(cl: ClassDecl) = prog.classes = cl::prog.classes

      def findClass(clazz: JavaClass): ClassDecl = {
        val name = clazz.getClassName

        val id = ClassIdentifier(convertName(name), List())
        val parent = convertParent(clazz.getSuperClass)
        val methods = clazz.getMethods.map(convertMethod(_, clazz)).toList
        ExternalClassDecl(id, parent, List(), methods)
      }

      def convertParent(parent: JavaClass) = parent match {
        case null   => None
        case parent =>
          addClass(findClass(parent))
          Some(ClassIdentifier(convertName(parent.getClassName), List()))
      }

      def convertMethod(meth: Method, clazz: JavaClass): MethodDecl = {
        val name = meth.getName match {
          case "<init>" => convertName(clazz.getClassName)
          case name     => convertName(name)
        }
        val id = Identifier(name)
        val retType = convertType(meth.getReturnType)
        val args = meth.getArgumentTypes.zipWithIndex.map{case (tpe, i) => Formal(convertType(tpe), Identifier("arg" + i))}.toList
        val access = convertAccess(meth.getAccessFlags)

        MethodDecl(retType, id, args, List(), List(), access)
      }

      def convertAccess(accessFlags: Int): Accessability = accessFlags match {
        case Constants.ACC_PUBLIC    => Public
        case Constants.ACC_PRIVATE   => Private
        case Constants.ACC_PROTECTED => Protected
        case _ => Private
      }

      def convertName(name: String): String =
        if(name == "java.lang.Object") "Object"
        else name.replaceAll("\\.", "/")

      def convertType(tpe: Type): TypeTree = tpe match {
        case x: BasicType => x match {
          case Type.BOOLEAN => BooleanType()
          case Type.INT => IntType()
          case Type.VOID => UnitType()
          case _ => IntType()
        }
        case x: ObjectType => x match {
          case Type.STRING => StringType()
          case Type.OBJECT => ClassIdentifier(convertName(x.getClassName))
          case _ => StringType()
        }
        case x: org.apache.bcel.generic.ArrayType => ArrayType(convertType(x.getBasicType))
      }
    }



    def createClassSet(program: Program): Set[String] = program.classes.map(_.id.value).toSet
    val originalClasses = createClassSet(prog)

    /**
     * Replaces the names of the classes declared in this file by
     * the name expected by the JVM for the given package. For example,
     * class Foo declared in package bar.baz will be replaced by bar/baz/Foo.
     */
    def replaceNames(prog: Program): Program = {
      prog.progPackage match {
        case Some(pack) =>
          val packString = prog.getPackageDirectory
          prog.main.id.value = packString + prog.main.id.value
          Trees.traverse(prog, (_, t) => Some(t) collect {
            case c: ClassIdentifier =>
              val name = c.value
              if(originalClasses(name)){
                addedClasses += packString + name
                c.value = packString + name
              }else if (!addedExternalClasses.contains(name) && !addedClasses(name)){
                println(addedClasses)
                println("Adding " + name)
                Importer.addClass(name)
              }
              if(addedExternalClasses.contains(name)){
                c.value = addedExternalClasses(name)
              }
            case c: ConstructorDecl => if(originalClasses(c.id.value)) c.id.value = packString + c.id.value
          })
        case None =>
      }
      prog
    }


    replaceNames(prog)
    println(Printer(prog))
    prog
  }
}
