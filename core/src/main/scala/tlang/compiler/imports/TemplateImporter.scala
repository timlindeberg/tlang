package tlang.compiler.imports

import tlang.Context
import tlang.compiler.ast.Parsing
import tlang.compiler.ast.Trees._
import tlang.compiler.lexer.Lexing
import tlang.messages.CompilationException
import tlang.utils.FileSource

import scala.collection.mutable

class TemplateImporter(ctx: Context, imported: mutable.Set[String] = mutable.Set()) {

  def classExists(importName: String): Boolean = ctx.classPath(importName).exists(_.isInstanceOf[TemplateFile])

  def importCUs(importName: String): List[CompilationUnit] = {
    if (imported(importName))
      return Nil

    imported += importName

    ctx.classPath(importName)
      .collect { case TemplateFile(path) => path }
      .flatMap { parseTemplateFile }
      .map { importedCU =>
        // Recursively import generics
        importedCU :: (importedCU.imports.imports flatMap { recursiveImport =>
          val templateImporter = new TemplateImporter(ctx, imported)
          templateImporter.importCUs(recursiveImport.name)
        })
      }
      .getOrElse(Nil)
  }

  private def parseTemplateFile(path: String): Option[CompilationUnit] = {
    val sources = FileSource(path) :: Nil
    try {
      val parsedProgram = (Lexing andThen Parsing).execute(ctx)(sources).head
      Some(parsedProgram)
    } catch {
      case e: CompilationException =>
        e.messages.print()
        None
    }
  }

}
