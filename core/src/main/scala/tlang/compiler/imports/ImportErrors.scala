package tlang.compiler.imports

import tlang.compiler.ast.Trees.ExtensionImport
import tlang.compiler.error.{Error, ErrorHandling, Warning}
import tlang.utils.Positioned

/**
  * Created by Tim Lindeberg on 5/14/2016.
  */
trait ImportErrors extends ErrorHandling {


  def report(error: Error): Unit = ctx.reporter.report(error)

  val ErrorLetters = "I"

  abstract class ImportError(code: Int, pos: Positioned) extends Error(ErrorLetters, code, pos)

  abstract class ImportWarning(code: Int, pos: Positioned) extends Warning(ErrorLetters, code, pos)

  //---------------------------------------------------------------------------------------
  //  Error messages
  //---------------------------------------------------------------------------------------

  case class CantResolveImport(imp: String, override val pos: Positioned) extends ImportError(0, pos) {
    lazy val message = err"Cannot resolve import $imp."
  }

  case class ConflictingImport(imp1: String, imp2: String, override val pos: Positioned) extends ImportError(1, pos) {
    lazy val message = err"Imports $imp1 and $imp2 are conflicting."
  }

  case class CantResolveExtensionsImport(imp: ExtensionImport, override val pos: Positioned) extends ImportError(2, pos) {
    lazy val message = err"Cannot resolve extension import ${imp.writtenName}"
  }

  case class DefaultImportDoesntExist(ignoredImport: String, override val pos: Positioned) extends ImportError(3, pos) {
    lazy val message = err"There is no default import called $ignoredImport."
  }


  //---------------------------------------------------------------------------------------
  //  Warnings
  //---------------------------------------------------------------------------------------

  case class NoGenerics(fileName: String, override val pos: Positioned) extends ImportWarning(0, pos) {
    lazy val message = err"Generic import $fileName did not contain any generic classes."
  }

}
