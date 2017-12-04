package tlang.options.arguments


import better.files.File
import tlang.formatting.Formatter
import tlang.messages.ErrorStringContext
import tlang.options.ArgumentFlag
import tlang.utils.Extensions._

case object DirectoryFlag extends ArgumentFlag[Set[File]] {
  override val name           = "directory"
  override val shortFlag      = Some("d")
  override val argDescription = "dir"

  override def description(formatter: Formatter): String = {
    "Specify a path where generated classes are placed."
  }

  override def verify(outDir: String)(implicit errorContext: ErrorStringContext): Unit = {
    import errorContext.ErrorStringContext

    if (!outDir.isValidPath)
      error(err"Invalid output directory: $outDir.")
  }

  override def parseValue(args: Set[String]): Set[File] = {
    if (args.isEmpty)
      return Set(File("."))

    args.map(File(_))
  }


}