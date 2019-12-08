package tlang
package compiler
package argument

import better.files.File
import tlang.formatting.Formatter
import tlang.options.ArgumentFlag


case object DirectoryFlag extends ArgumentFlag[Set[File]] {
  override val name = "directory"
  override val shortFlag = Some("d")
  override val argDescription = "dir"

  override def description(implicit formatter: Formatter): String =
    s"""
       |Specify paths where generated classes are placed.
       |Defaults to ${ highlight(".") }
      """

  override def parseValue(args: Set[String]): Set[File] = {
    if (args.isEmpty) Set(File(".")) else args.map(File(_))
  }


}
