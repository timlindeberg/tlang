package tcompiler.modification

import tcompiler.utils.{Errors, Positioned}

/**
  * Created by Tim Lindeberg on 6/22/2016.
  */
trait TemplateErrors extends Errors {

  override val ErrorPrefix = "G"

  private def error(errorCode: Int, msg: String, pos: Positioned): Unit =
    ctx.reporter.error(ErrorPrefix, errorCode, msg, pos, importMap)

  //---------------------------------------------------------------------------------------
  //  Error messages
  //---------------------------------------------------------------------------------------

  protected def ErrorWrongNumGenerics(expected: Int, found: Int, pos: Positioned): Unit =
    error(0, s"Wrong number of template parameters, expected '$expected', found '$found'.", pos)

  protected def ErrorDoesNotExist(name: String, pos: Positioned): Unit =
    error(1, s"Can not find template class named '$name'.", pos)

  protected def ErrorSameName(name: String, pos: Positioned): Unit =
    error(2, s"Generic parameter duplicate: '$name'.", pos)

}
