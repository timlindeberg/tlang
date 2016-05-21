package tcompiler.imports

import java.io.File

import tcompiler.Main
import tcompiler.ast.Parser
import tcompiler.ast.Trees.{Import, Program, TemplateImport}
import tcompiler.lexer.Lexer
import tcompiler.utils.{CompilationException, Context}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
  * Created by Tim Lindeberg on 5/15/2016.
  */
class TemplateImporter(
  override var ctx: Context,
  prog: Program,
  imported: mutable.Set[String] = mutable.Set()) extends ImportErrors
{

  def apply(): List[Program] = {
    val importedPrograms: ArrayBuffer[Program] = ArrayBuffer()
    val genericImports = prog.imports.filter(_.isInstanceOf[TemplateImport])

    genericImports.foreach { imp =>
      val fileName = mkImportString(imp, "/") + Main.FileEnding
      if (!imported(fileName)) {
        val newClasses = importProgram(fileName, imp)
        if (newClasses.nonEmpty) {
          importedPrograms ++= newClasses
          imported += fileName
        } else {
          ErrorResolvingGenericImport(mkImportString(imp, "."), imp)
        }
      }
    }
    importedPrograms.toList
  }

  private def importProgram(fileName: String, imp: Import): List[Program] = {
    val importedPrograms: ArrayBuffer[Program] = ArrayBuffer()
    ctx.getClassPaths.foreach { path =>
      val file = new File(path + "/" + fileName)
      if (file.exists()) {
        parseGenericFile(ctx, file) match {
          case Some(importedProg) =>
            importedPrograms += importedProg

            // Recursively import generics
            val templateImporter = new TemplateImporter(ctx, importedProg, imported)
            importedPrograms ++= templateImporter()
          case None =>
        }
        return importedPrograms.toList
      }
    }
    List()
  }

  private def mkImportString(imp: Import, sep: String) = {
    val id = imp.identifiers
    if (id.length == 1) {
      prog.getPackageDirectory + "/" + id.head.value
    } else {
      imp.identifiers.map(_.value).mkString(sep)
    }
  }

  private def parseGenericFile(ctx: Context, file: File): Option[Program] =
    try {
      Some((Lexer andThen Parser).run(ctx)(List(file)).head)
    } catch {
      case e: CompilationException =>
        println(e.getMessage)
        None
    }
}
