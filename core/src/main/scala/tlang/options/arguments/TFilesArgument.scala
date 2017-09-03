package tlang.options.arguments


import better.files.File
import tlang.Constants
import tlang.messages.ErrorStringContext
import tlang.options.PositionalArgument

case object TFilesArgument extends PositionalArgument[Set[File]] {

  override def verifyArgument(path: String)(implicit errorContext: ErrorStringContext): Unit = {
    import errorContext.ErrorStringContext

    val file = File(path)
    if (!file.exists())
      error(err"No such file: ${ file.path }.")

    if (file.isDirectory) {
      val tFiles = file.glob("*" + Constants.FileEnding)
      if (tFiles.isEmpty)
        error(err"The given directory $path does not contain any T-files.")

    } else if (!file.extension.contains(Constants.FileEnding)) {
      error(err"The given file $path is not a T-file.")
    }
  }

  override def parseValue(args: Set[String]): Set[File] = {
    args.flatMap { path =>

      val file = File(path)
      if (file.isDirectory) file.glob("*" + Constants.FileEnding) else List(file)
    }
  }

}