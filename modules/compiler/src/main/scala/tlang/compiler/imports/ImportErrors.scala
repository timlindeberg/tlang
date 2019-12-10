package tlang
package compiler
package imports

import tlang.compiler.ast.Trees.Import
import tlang.compiler.messages.{ErrorHandling, ErrorMessage, ExtraMessage, WarningMessage}
import tlang.utils.Positioned

trait ImportErrors extends ErrorHandling {

  def report(error: ErrorMessage): Unit = reporter.report(error)

  val ErrorLetters = "I"

  import errorStringContext._

  abstract class ImportError(code: Int, pos: Positioned) extends ErrorMessage(ErrorLetters, code, pos)

  abstract class ImportWarning(code: Int, pos: Positioned) extends WarningMessage(ErrorLetters, code, pos)

  //---------------------------------------------------------------------------------------
  //  Error messages
  //---------------------------------------------------------------------------------------

  case class CantResolveImport(imp: String, override val pos: Positioned) extends ImportError(0, pos) {
    lazy val message = err"Could not resolve import $imp."
  }

  case class AlreadyImported(imp1: Import, imp2: Import, override val pos: Positioned) extends ImportError(1, pos) {
    lazy val message = err"${ imp1.writtenName } is already imported"

    case class OtherImport() extends ExtraMessage(imp2) {
      lazy val message: String = err"Here:"
    }
    override lazy val notes = List(OtherImport())
  }

  case class AlreadyImportedByDefault(imp: Import) extends ImportError(2, imp) {
    lazy val message = err"${ imp.writtenName } is already imported by default."
  }

  case class ConflictingImport(imp1: String, imp2: String, override val pos: Positioned) extends ImportError(3, pos) {
    lazy val message = err"Imports $imp1 and $imp2 are conflicting."
  }

  //---------------------------------------------------------------------------------------
  //  Warnings
  //---------------------------------------------------------------------------------------

  case class NoGenerics(fileName: String, override val pos: Positioned) extends ImportWarning(0, pos) {
    lazy val message = err"Generic import $fileName did not contain any generic classes."
  }

}
