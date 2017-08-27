package tlang.options.arguments

import java.io.File

import tlang.Constants
import tlang.compiler.error.ErrorStringContext
import tlang.options.PositionalArgument

case object TFilesArgument extends PositionalArgument[Set[File]] {

  override def verifyArgument(path: String)(implicit errorContext: ErrorStringContext): Unit = {
    import errorContext.ErrorStringContext

    val file = new File(path)
    if (!file.exists())
      error(err"No such file: ${ file.getPath }.")

    if (file.isDirectory) {
      val tFiles = file.listFiles().filter(_.getName.endsWith(Constants.FileEnding))
      if (tFiles.isEmpty)
        error(err"The given directory $path does not contain any T-files.")

    } else if (!file.getName.endsWith(Constants.FileEnding)) {
      error(err"The given file $path is not a T-file.")
    }
  }

  override def parseValue(args: Set[String]): Set[File] = {
    args.flatMap { path =>

      val file = new File(path)
      if (file.isDirectory) file.listFiles.filter(_.getName.endsWith(Constants.FileEnding)) else List(file)
    }
  }

}