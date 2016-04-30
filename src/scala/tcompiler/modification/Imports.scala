package tcompiler
package modification

import java.io.File
import org.apache.bcel._
import org.apache.bcel.classfile.{JavaClass, Method, _}
import org.apache.bcel.generic.{BasicType, ObjectType, Type}
import tcompiler.ast.Trees._
import tcompiler.ast.{Parser, Trees}
import tcompiler.lexer.Lexer
import tcompiler.utils.{CompilationException, Context, Pipeline, Positioned}

import scala.collection.mutable.ArrayBuffer

object Imports extends Pipeline[Program, Program] {

  val LocationPrefix = "I"

  def run(ctx: Context)(prog: Program): Program = {
    new NameReplacer(ctx, prog).replaceNames
  }

}

class NameReplacer(ctx: Context, prog: Program) {

  import Imports._

  private val originalClasses = prog.classes.map(_.id.value).toSet

  private var addedExternalClasses = Map[String, String]()
  private var addedClasses = Set[String]()
  private var triedToImport = Set[String]()
  private val importer = new Importer(ctx, prog)

  /**
   * Replaces the names of the classes declared in this file by
   * the name expected by the JVM for the given package. For example,
   * class Foo declared in package bar.baz will be replaced by bar/baz/Foo.
   */
  def replaceNames: Program = {
    val packString = prog.getPackageDirectory

    Trees.traverse(prog, (_, t) => Some(t) collect {
      case c: ClassIdentifier =>
        handleImport(packString, c.value) collect { case newName => c.value = newName }
      case c: ConstructorDecl => if (originalClasses(c.id.value)) c.id.value = packString + c.id.value
    })

    prog.classes.foreach {
      _.methods.foreach { meth =>
        // TODO: FIX THIS SHIT
          Trees.traverse(meth.stat, (_, t) => Some(t) collect {
            case MethodCall(id @ Identifier(name), _, _s) =>
              // Static call
            case New(ClassIdentifier(name, _), _) =>
            case NewArray(ClassIdentifier(name, _), _) =>
            case id: Identifier if id.value.charAt(0).isUpper =>
              handleImport(packString, id.value) collect {
                case newName => id.value = newName // TODO: More effecient way of handling imports for regular identifiers?
              }
          })
      }
    }
    checkUnusedImports()

    importStandardClasses()

    prog
  }

  private def handleImport(packString: String, name: String): Option[String] = {
    val packString = prog.getPackageDirectory

    if (originalClasses(name)) {
      addedClasses += packString + name
      return Some(packString + name)
    }

    if (!triedToImport(name) && !addedExternalClasses.contains(name) && !addedClasses(name)) {
      importer.importClass(name) match {
        case Some(added) => addedExternalClasses += name -> added
        case _           => triedToImport += name
      }
    }
    if (addedExternalClasses.contains(name)) {
      return Some(addedExternalClasses(name))
    }
    None
  }


  private def checkUnusedImports() = {
    prog.imports.foreach(imp => {
      if (!importer.usedImports.contains(imp) && !imp.isInstanceOf[GenericImport]) {
        val importName = imp.identifiers.map(_.value).mkString("/")
        WarningUnusedImport(importName, imp)
      }
    })
  }

  private def importStandardClasses() = {

  }

  private def WarningUnusedImport(name: String, pos: Positioned) =
    ctx.reporter.warning(LocationPrefix, 0, s"Unused import '$name'.", pos)


}

class Importer(ctx: Context, prog: Program) {

  var usedImports = Set[Import]()

  val regImports = prog.imports.filter(_.isInstanceOf[RegularImport]).asInstanceOf[List[RegularImport]]
  val wcImports = prog.imports.filter(_.isInstanceOf[WildCardImport]).asInstanceOf[List[WildCardImport]]


  def importClass(className: String): Option[String] = {
    if (className == "")
      return None

    val fullName = className.replaceAll("\\.", "/")
    if (addImport(fullName)) {
      return Some(fullName)
    }

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

  private def addImport(className: String) = {
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
    }

  private def findClass(clazz: JavaClass): ClassDecl = {
    val name = clazz.getClassName

    val id = ClassIdentifier(convertName(name), List())
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
      case parent if parent.getClassName == "java.lang.Object" => List()
      case parent                                              =>
        prog.classes = findClass(parent) :: prog.classes
        List(ClassIdentifier(convertName(parent.getClassName), List()))
    }
  }

  private def convertField(field: Field) =
      new VarDecl(Some(convertType(field.getType)), Identifier(field.getName), None, convertModifiers(field))

  private def convertMethod(meth: Method, clazz: JavaClass): MethodDecl = {
    val name = meth.getName match {
      case "<init>" => "new"
      case name     => convertName(name)
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

  private def convertName(name: String): String =
    if (name == "java.lang.Object") "Object"
    else name.replaceAll("\\.", "/")

  private def convertType(tpe: Type): TypeTree = tpe match {
    case x: BasicType                         => x match {
      case Type.BOOLEAN => BooleanType()
      case Type.INT     => IntType()
      case Type.CHAR    => CharType()
      case Type.LONG    => LongType()
      case Type.FLOAT   => FloatType()
      case Type.DOUBLE  => DoubleType()
      case Type.VOID    => UnitType()
      case _            => IntType()
    }
    case x: ObjectType                        => x match {
      case Type.STRING => StringType()
      case Type.OBJECT => ClassIdentifier(convertName(x.getClassName))
      case _           => StringType()
    }
    case x: org.apache.bcel.generic.ArrayType => ArrayType(convertType(x.getBasicType))
  }
}




class GenericImporter(ctx: Context, prog: Program, imported: scala.collection.mutable.Set[String] = scala.collection.mutable.Set()) {

  import Imports._

  def importGenericClasses: List[ClassDecl] = {
    val importedClasses: ArrayBuffer[ClassDecl] = ArrayBuffer()
    val genericImports = prog.imports.filter(_.isInstanceOf[GenericImport])
    genericImports.foreach { imp =>
      val fileName = mkImportString(imp, "/") + ".kool"
      if(!imported(fileName)){
        val newClasses = importClass(fileName, imp)
        if(newClasses.nonEmpty){
          importedClasses ++= newClasses
          imported += fileName
        }else{
          ErrorResolvingGenericImport(mkImportString(imp, "."), imp)
        }
      }
    }
    importedClasses.toList
  }

  private def importClass(fileName: String, imp: Import): ArrayBuffer[ClassDecl] = {
    val importedClasses: ArrayBuffer[ClassDecl] = ArrayBuffer()
    getClassPaths.foreach { path =>
      val file = new File(path + "/" + fileName)
      if (file.exists()) {
        parseGenericFile(ctx, file) match {
          case Some(importedProg) =>
            // Recursively import generics
            val genericsInImportedProg = new GenericImporter(ctx, importedProg, imported).importGenericClasses
            val genericClasses = importedProg.classes.filter(_.id.isTemplated) ::: genericsInImportedProg
            prog.imports = prog.imports ::: importedProg.imports.filter(!_.isInstanceOf[GenericImport]) // Add imports as a side effect, ugly
            if (genericClasses.nonEmpty)
              importedClasses ++= genericClasses
            else
              WarningNoGenerics(file.getName, imp)

          case None => ErrorImportParsing(file.getName, imp)
        }
        return importedClasses
      }
    }
    importedClasses
  }

  private def mkImportString(imp: Import, sep: String) = {
    val id = imp.identifiers
    if (id.length == 1) {
      prog.getPackageDirectory + id.head.value
    } else {
      imp.identifiers.map(_.value).mkString(sep)
    }
  }

  private def getClassPaths: List[String] = {
    if(!sys.env.contains("T_HOME"))
      return List("", "C:/Users/Tim Lindeberg/IdeaProjects/T-Compiler/src/stdlib")

    List("", sys.env("T_HOME"))
  }

  private def parseGenericFile(ctx: Context, file: File): Option[Program] =
    try {
      Some((Lexer andThen Parser).run(ctx)(file))
    } catch {
      case _: CompilationException => None
    }

  private def error(errorCode: Int, msg: String, pos: Positioned) =
    ctx.reporter.error(LocationPrefix, errorCode, msg, pos)

  private def warning(errorCode: Int, msg: String, pos: Positioned) =
    ctx.reporter.warning(LocationPrefix, errorCode, msg, pos)

  private def ErrorImportParsing(fileName: String, pos: Positioned) =
    error(0, s"Found parse error in import '$fileName'.", pos)

  private def ErrorResolvingGenericImport(imp: String, pos: Positioned) =
    error(1, s"Could not resolve generic import '$imp'.", pos)

  private def WarningNoGenerics(fileName: String, pos: Positioned) =
    warning(1, s"Generic import '$fileName' did not contain any generic classes.", pos)

}
