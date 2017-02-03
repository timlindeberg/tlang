package tcompiler

import tcompiler.Flags.Flag
import tcompiler.error.{Errors, NameSuggestor}
import tcompiler.imports.ImportMap
import tcompiler.utils.Context

/**
  * Created by Tim Lindeberg on 5/13/2016.
  */
trait MainErrors extends Errors {

  override var ctx      : Context   = _
  override val ErrorLetters         = "M"
  override var importMap: ImportMap = _

  private val nameSuggestor = new NameSuggestor


  private def fatal(message: String) = {
    println(message)
    sys.exit(1)
  }

  //---------------------------------------------------------------------------------------
  // Errors
  //---------------------------------------------------------------------------------------

  protected def FatalWrongNumFilesGiven(numFiles: Int): Nothing =
    fatal(s"Exactly one file expected, '$numFiles' file(s) given.")

  protected def FatalCannotFindFile(fileName: String): Nothing =
    fatal(s"Cannot find file '$fileName'.")

  protected def FatalNoFilesGiven(): Nothing =
    fatal(s"No files given.")

  protected def FatalInvalidOutputDirectory(outDir: String): Nothing =
    fatal(s"Invalid output directory: '$outDir'.")

  protected def FatalOutputDirectoryCouldNotBeCreated(outDir: String): Nothing =
    fatal(s"Output directory '$outDir' does not exist and could not be created.")

  protected def FatalInvalidClassPath(classPath: String): Nothing =
    fatal(s"Invalid output class path: '$classPath'.")

  protected def FatalCantFindTHome(tHome: String): Nothing =
    fatal(s"$tHome environment variable is not set. It needs to point to the directory of the T standard library.")

  protected def FatalInvalidTHomeDirectory(path: String, tHome: String): Nothing =
    fatal(s"'$path' is not a valid $tHome directory.")

  protected def FatalGivenDirectoryContainsNoTFiles(path: String): Nothing =
    fatal(s"The given directory '$path' does not contain any T-files.")

  protected def FatalInvalidFlag(flag: String): Nothing =
    fatal(s"'$flag' is not a valid flag. Type --help to see what type of input is valid.")

  protected def FatalGivenFileIsNotTFile(path: String): Nothing =
    fatal(s"The given file '$path' is not a T-file.")

  protected def FatalInvalidArgToFlag(flag: Flag, arg: String, alternatives: List[String]): Nothing =
    fatal(s"'$arg' is not a valid argument to flag '--${flag.flag}'.${nameSuggestor(arg, alternatives)}")

}
