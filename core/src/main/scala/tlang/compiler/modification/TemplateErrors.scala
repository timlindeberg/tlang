package tlang.compiler.modification

import tlang.compiler.error.{ErrorLevel, Errors}
import tlang.compiler.utils.Positioned

/**
  * Created by Tim Lindeberg on 6/22/2016.
  */
trait TemplateErrors extends Errors {

  override val ErrorLetters = "G"

  private def error(errorCode: Int, msg: String, pos: Positioned): Unit =
    report(errorCode, msg, ErrorLevel.Error, pos)

  //---------------------------------------------------------------------------------------
  //  Error messages
  //---------------------------------------------------------------------------------------

  protected def ErrorWrongNumGenerics(expected: Int, found: Int, pos: Positioned): Unit =
    error(0, err"Wrong number of template parameters, expected $expected, found $found.", pos)

  protected def ErrorDoesNotExist(name: String, pos: Positioned): Unit =
    error(1, err"Can not find template class named $name.", pos)

  protected def ErrorSameName(name: String, pos: Positioned): Unit =
    error(2, err"Generic parameter $name appears multiple times.", pos)

}
