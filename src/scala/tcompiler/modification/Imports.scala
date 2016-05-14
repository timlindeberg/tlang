package tcompiler
package modification

import java.io.File

import tcompiler.ast.Trees._
import tcompiler.ast.{Parser, Trees}
import tcompiler.imports.Importer
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
  private var addedClasses         = Set[String]()
  private var triedToImport        = Set[String]()
  private val importer             = new Importer(ctx, prog)

  /**
    * Replaces the names of the classes declared in this file by
    * the name expected by the JVM for the given package. For example,
    * class Foo declared in package bar.baz will be replaced by bar/baz/Foo.
    */
  def replaceNames: Program = {
    val packString = prog.getPackageDirectory.replaceAll("/", ".")


    Trees.traverse(prog, (_, t) => Some(t) collect {
      case c: ClassIdentifier =>
        handleImport(packString, c.value) collect { case newName => c.value = newName }
      case c: ConstructorDecl => if (originalClasses(c.id.value)) c.id.value = packString + c.id.value
    })

    prog.classes.foreach {
      _.methods.foreach { meth =>
        // TODO: FIX THIS SHIT
        Trees.traverse(meth.stat, (_, t) => Some(t) collect {
          case MethodCall(id@Identifier(name), _, _s) =>
          // Static call
          case New(ClassIdentifier(name, _), _)             =>
          case NewArray(ClassIdentifier(name, _), _)        =>
          case id: Identifier if id.value.charAt(0).isUpper =>
            handleImport(packString, id.value) collect {
              case newName => id.value = newName // TODO: More effecient way of handling imports for regular identifiers?
            }
        })
      }
    }
    addSecondClassImports()
    checkUnusedImports()

    importStandardClasses()

    prog
  }

  private def addSecondClassImports() = {
    val classes = importer.secondClassImports.
      filter(name => !prog.classes.exists(_.id.value == name)).
      map(name => ExternalClassDecl(ClassIdentifier(name), List(), List(), List())).toList
    prog.classes :::= classes
  }

  private def handleImport(packString: String, name: String): Option[String] = {
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
      Some(addedExternalClasses(name))
    }else{
      None
    }
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

class GenericImporter(ctx: Context, prog: Program, imported: scala.collection.mutable.Set[String] = scala.collection.mutable.Set()) {

  import Imports._

  def importGenericClasses: List[ClassDecl] = {
    val importedClasses: ArrayBuffer[ClassDecl] = ArrayBuffer()
    val genericImports = prog.imports.filter(_.isInstanceOf[GenericImport])
    genericImports.foreach { imp =>
      val fileName = mkImportString(imp, "/") + ".kool"
      if (!imported(fileName)) {
        val newClasses = importClass(fileName, imp)
        if (newClasses.nonEmpty) {
          importedClasses ++= newClasses
          imported += fileName
        } else {
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

          case None =>
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
    List("", Main.TDirectory)
  }

  private def parseGenericFile(ctx: Context, file: File): Option[Program] =
    try {
      Some((Lexer andThen Parser).run(ctx)(file))
    } catch {
      case e: CompilationException =>
        println(e.getMessage)
        None
    }

  private def error(errorCode: Int, msg: String, pos: Positioned) =
    ctx.reporter.error(LocationPrefix, errorCode, msg, pos)

  private def warning(errorCode: Int, msg: String, pos: Positioned) =
    ctx.reporter.warning(LocationPrefix, errorCode, msg, pos)

  private def ErrorResolvingGenericImport(imp: String, pos: Positioned) =
    error(1, s"Could not resolve generic import '$imp'.", pos)

  private def WarningNoGenerics(fileName: String, pos: Positioned) =
    warning(1, s"Generic import '$fileName' did not contain any generic classes.", pos)

}
