package tcompiler

import tcompiler.Flags.MaxErrors
import tcompiler.imports.ImportMap
import tcompiler.utils.{Context, Errors}

/**
  * Created by Tim Lindeberg on 5/13/2016.
  */
trait MainErrors extends Errors {

  override var ctx: Context = null
  override val ErrorPrefix = "M"
  override var importMap = new ImportMap()

  private def fatal(message: String) = {
    println(message)
    sys.exit(1)
  }

  //---------------------------------------------------------------------------------------
  // Errors
  //---------------------------------------------------------------------------------------

  protected def FatalWrongNumFilesGiven(numFiles: Int) =
    fatal(s"Exactly one file expected, '$numFiles' file(s) given.")

  protected def FatalCannotFindFile(fileName: String) =
    fatal(s"Cannot find file '$fileName'.")

  protected def FatalInvalidOutputDirectory(outDir: String) =
    fatal(s"Invalid output directory: '$outDir'.")

  protected def FatalOutputDirectoryCouldNotBeCreated(outDir: String) =
    fatal(s"Output directory '$outDir' does not exist and could not be created.")

  protected def FatalInvalidClassPath(classPath: String) =
    fatal(s"Invalid output class path: '$classPath'.")

  protected def FatalCantFindTHome(tHome: String) =
    fatal(s"$tHome environment variable is not set.")

  protected def FatalInvalidTHomeDirectory(path: String, tHome: String) =
    fatal(s"'$path' is not a valid $tHome directory.")

  protected def FatalInvalidMaxErrors(num: String) =
    fatal(s"'$num' is not a valid argument to the flag '${MaxErrors.flag}'. Needs a number as argument.")

}
