package tlang
package compiler
package imports

import tlang.compiler.ast.Parsing
import tlang.compiler.ast.Trees._
import tlang.compiler.lexer.Lexing
import tlang.compiler.messages.CompilationException
import tlang.compiler.output.ErrorMessageOutput
import tlang.compiler.utils.TLangSyntaxHighlighter
import tlang.options.argument.MessageContextFlag
import tlang.utils.{FileSource, Logging}

import scala.collection.mutable

class TemplateImporter(ctx: Context, imported: mutable.Set[String] = mutable.Set()) extends Logging {

  import ctx.formatter

  implicit val syntaxHighlighter: TLangSyntaxHighlighter = TLangSyntaxHighlighter()

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
    info"Parsing template file $path"

    val sources = FileSource(path) :: Nil
    try {
      // Copy context so we don't reset execution times of the old context
      val newContext = ctx.copy(reporter = ctx.reporter.cleared())
      val parsedProgram = (Lexing andThen Parsing).execute(newContext)(sources).head
      Some(parsedProgram)
    } catch {
      case e: CompilationException =>
        ctx.output += ErrorMessageOutput(e.messages, ctx.options(MessageContextFlag))
        None
    }
  }
}
