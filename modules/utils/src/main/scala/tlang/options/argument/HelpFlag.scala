package tlang.options.argument

import tlang.formatting.Formatter
import tlang.options.{FlagArgument, OptionalArgumentFlag}
import tlang.utils.Extensions._

// All flags is a function so we can pass the list of compiler flags to the help flag.
// Otherwise we get a null pointer exception since the Help Flag is part of the compiler flags
// list and gets initialized before the list exists.
object HelpFlag {
  val name           = "help"
  val shortFlag      = Some("h")
  val argDescription = "about"
  val defaultArg     = "all"
}

class HelpFlag(allFlags: => Set[FlagArgument[_]]) extends OptionalArgumentFlag[Set[String]] {
  override val name           = HelpFlag.name
  override val shortFlag      = HelpFlag.shortFlag
  override val argDescription = HelpFlag.argDescription
  override val defaultArg     = HelpFlag.defaultArg

  val Phases: String = "phases"

  private lazy val FlagNames : Set[String] = allFlags.map(_.name)
  private lazy val OtherNames: Set[String] = Set(Phases)
  private lazy val ValidArguments          = FlagNames ++ OtherNames

  override def isValidArg(arg: String): Boolean = arg in ValidArguments

  override def description(formatter: Formatter): String = {
    s"""
       |Prints help information and exits. Giving a flag as argument will give more information about that flag.
       |--help phases prints information about the different phases of the T-Compiler.
      """.stripMargin.trim
  }

  override def parseValue(args: Set[String]): Set[String] = args

}