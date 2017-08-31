package tlang.compiler

import tlang.messages.AlternativeSuggestor

trait MainErrors {

  val nameSuggestor = new AlternativeSuggestor

  private def fatal(message: String) = {
    println(message)
    sys.exit(1)
  }

  //---------------------------------------------------------------------------------------
  // Errors
  //---------------------------------------------------------------------------------------

  protected def ErrorNoFilesGiven(): Nothing =
    fatal(s"No files given.")

  protected def FatalInvalidTHomeDirectory(path: String, tHome: String): Nothing =
    fatal(s"'$path' is not a valid $tHome directory.")

}
