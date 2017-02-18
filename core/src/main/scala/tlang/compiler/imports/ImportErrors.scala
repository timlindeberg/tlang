package tlang.compiler.imports

import tlang.compiler.ast.Trees.ExtensionImport
import tlang.compiler.error.{ErrorLevel, Errors}
import tlang.compiler.utils.Positioned

/**
  * Created by Tim Lindeberg on 5/14/2016.
  */
trait ImportErrors extends Errors {

  override val ErrorLetters = "I"

  def error(errorCode: Int, msg: String, pos: Positioned): Unit =
    report(errorCode, msg, ErrorLevel.Error, pos)


  //---------------------------------------------------------------------------------------
  //  Error messages
  //---------------------------------------------------------------------------------------

  protected def ErrorCantResolveImport(imp: String, pos: Positioned): Unit =
    error(0, err"Cannot resolve import $imp.", pos)

  protected def ErrorConflictingImport(imp1: String, imp2: String, pos: Positioned): Unit =
    error(1, err"Imports $imp1 and $imp2 are conflicting.", pos)

  protected def ErrorCantResolveExtensionsImport(imp: ExtensionImport, pos: Positioned): Unit =
    error(2, err"Cannot resolve extension import $imp.", pos)

  protected def ErrorDefaultImportDoesntExist(ignoredImport: String, pos: Positioned): Unit =
    error(3, err"There is no default import called $ignoredImport.", pos)


  //---------------------------------------------------------------------------------------
  //  Warnings
  //---------------------------------------------------------------------------------------

  protected def WarningNoGenerics(fileName: String, pos: Positioned): Unit =
    warning(0, err"Generic import $fileName did not contain any generic classes.", pos)

}
