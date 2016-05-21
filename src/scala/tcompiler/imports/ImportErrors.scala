package tcompiler.imports

import tcompiler.utils.{Errors, Positioned}

/**
  * Created by Tim Lindeberg on 5/14/2016.
  */
trait ImportErrors extends Errors {

  override val ErrorPrefix = "I"

  def error(errorCode: Int, msg: String, tree: Positioned) =
    ctx.reporter.error(ErrorPrefix, errorCode, msg, tree)

  //---------------------------------------------------------------------------------------
  //  Error messages
  //---------------------------------------------------------------------------------------

  protected def ErrorCantResolveImport(imp: String, pos: Positioned) =
    error(0, s"Cannot resolve import '$imp'.", pos)

  protected def ErrorConflictingImport(imp1: String, imp2: String, pos: Positioned) =
    error(1, s"Imports '$imp1' and '$imp2' are conflicting.", pos)

  protected def ErrorResolvingGenericImport(imp: String, pos: Positioned) =
    error(2, s"Could not resolve generic import '$imp'.", pos)

  //---------------------------------------------------------------------------------------
  //  Warnings
  //---------------------------------------------------------------------------------------

  protected def WarningNoGenerics(fileName: String, pos: Positioned) =
    warning(0, s"Generic import '$fileName' did not contain any generic classes.", pos)


}
