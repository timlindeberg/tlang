package tlang.compiler.argument

import better.files.File
import tlang.formatting.Formatter
import tlang.options.ArgumentFlag


case object DirectoryFlag extends ArgumentFlag[Set[File]] {
  override val name           = "directory"
  override val shortFlag      = Some("d")
  override val argDescription = "dir"

  override def description(formatter: Formatter): String = {
    "Specify paths where generated classes are placed."
  }

  override def parseValue(args: Set[String]): Set[File] = {
    if (args.isEmpty)
      return Set(File("."))

    args.map(File(_))
  }


}
