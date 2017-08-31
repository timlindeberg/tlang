package tlang.compiler.modification

import tlang.messages.{ErrorHandling, ErrorMessage}
import tlang.utils.Positioned

trait TemplateErrors extends ErrorHandling {

  //---------------------------------------------------------------------------------------
  //  Error messages
  //---------------------------------------------------------------------------------------

  import errorStringContext._


  def report(error: ErrorMessage): Unit = reporter.report(error)

  abstract class TemplateError(code: Int, pos: Positioned) extends ErrorMessage("G", code, pos)

  case class WrongNumGenerics(expected: Int, found: Int, override val pos: Positioned) extends TemplateError(0, pos) {
    lazy val message = err"Wrong number of template parameters, expected $expected, found $found."
  }

  case class ClassDoesNotExist(name: String, override val pos: Positioned) extends TemplateError(1, pos) {
    lazy val message = err"Can not find template class named $name."
  }

  case class SameName(name: String, override val pos: Positioned) extends TemplateError(2, pos) {
    lazy val message = err"Generic parameter $name appears multiple times."
  }

}
 