package tlang.compiler.modification

import tlang.compiler.error.{Error, ErrorHandling}
import tlang.compiler.utils.Positioned

/**
  * Created by Tim Lindeberg on 6/22/2016.
  */
trait TemplateErrors extends ErrorHandling {

  //---------------------------------------------------------------------------------------
  //  Error messages
  //---------------------------------------------------------------------------------------


  def report(error: Error): Unit = ctx.reporter.report(error)


  abstract class TemplateError(code: Int, pos: Positioned) extends Error("G", code, pos)

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
