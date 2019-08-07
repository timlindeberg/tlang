package tlang
package compiler
package imports

import tlang.compiler.messages.{ErrorHandling, ErrorMessage, WarningMessage}
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
    lazy val message = err"Cannot resolve import $imp."
  }

  case class ConflictingImport(imp1: String, imp2: String, override val pos: Positioned) extends ImportError(1, pos) {
    lazy val message = err"Imports $imp1 and $imp2 are conflicting."
  }

  //---------------------------------------------------------------------------------------
  //  Warnings
  //---------------------------------------------------------------------------------------

  case class NoGenerics(fileName: String, override val pos: Positioned) extends ImportWarning(0, pos) {
    lazy val message = err"Generic import $fileName did not contain any generic classes."
  }

}
