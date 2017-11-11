package tlang.options.arguments

import tlang.formatting.Formatter
import tlang.options.{FlagArgument, OptionalArgumentFlag}
import tlang.utils.Extensions._

// All flags is a function so we can pass the list of compiler flags to the help flag.
// Otherwise we get a null pointer exception since the Help Flag is part of the compiler flags
// list and gets initialized before the list exists.
class HelpFlag(allFlags: => Set[FlagArgument[_]]) extends OptionalArgumentFlag[Set[String]] {
  override val name           = "help"
  override val shortFlag      = Some("h")
  override val argDescription = "about"
  override val defaultArg     = "all"

  private lazy val FlagNames: Set[String] = allFlags.map(_.name)

  override def isValidArg(arg: String): Boolean = arg in FlagNames

  override def description(formatter: Formatter): String = {
    s"""
       |Prints help information and exits. Giving a flag as argument will give more information about that flag.
      """.stripMargin.trim
  }

  override def parseValue(args: Set[String]): Set[String] = args

}

case object CompilerHelpFlag extends HelpFlag(tlang.compiler.Main.CompilerFlags)
case object ReplHelpFlag extends HelpFlag(tlang.repl.Main.ReplFlags)
