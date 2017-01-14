package tcompiler.imports

import tcompiler.ast.Trees.ExtensionImport
import tcompiler.utils.{Errors, Positioned}

/**
  * Created by Tim Lindeberg on 5/14/2016.
  */
trait ImportErrors extends Errors {

  override val ErrorPrefix = "I"

  def error(errorCode: Int, msg: String, tree: Positioned): Unit =
    ctx.reporter.error(ErrorPrefix, errorCode, msg, tree, importMap)

  //---------------------------------------------------------------------------------------
  //  Error messages
  //---------------------------------------------------------------------------------------

  protected def ErrorCantResolveImport(imp: String, pos: Positioned): Unit =
    error(0, s"Cannot resolve import '$imp'.", pos)

  protected def ErrorConflictingImport(imp1: String, imp2: String, pos: Positioned): Unit =
    error(1, s"Imports '$imp1' and '$imp2' are conflicting.", pos)

  protected def ErrorCantResolveExtensionsImport(imp: ExtensionImport, pos: Positioned): Unit =
    error(2, s"Cannot resolve extension import '$imp'.", pos)

  protected def ErrorDefaultImportDoesntExist(ignoredImport: String, pos: Positioned): Unit =
    error(3, s"There is no default import called '$ignoredImport'.", pos)


  //---------------------------------------------------------------------------------------
  //  Warnings
  //---------------------------------------------------------------------------------------

  protected def WarningNoGenerics(fileName: String, pos: Positioned): Unit =
    warning(0, s"Generic import '$fileName' did not contain any generic classes.", pos)

}
