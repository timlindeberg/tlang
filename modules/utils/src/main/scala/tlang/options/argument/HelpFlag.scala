package tlang.options.argument

import tlang.formatting.Formatter
import tlang.options.{FlagArgument, OptionalArgumentFlag}
import tlang.utils.Extensions._

object HelpFlag {
  val Name           = "help"
  val ShortFlag      = Some("h")
  val ArgDescription = "about"
  val DefaultArg     = "all"
}

// All flags is a function so we can pass the list of compiler flags to the help flag.
// Otherwise we get a null pointer exception since the Help Flag is part of the compiler flags
// list and gets initialized before the list exists.
class HelpFlag(allFlags: => Set[FlagArgument[_]]) extends OptionalArgumentFlag[Set[String]] {
  override val name          : String       = HelpFlag.Name
  override val shortFlag     : Some[String] = HelpFlag.ShortFlag
  override val argDescription: String       = HelpFlag.ArgDescription
  override val defaultArg    : String       = HelpFlag.DefaultArg

  val Phases: String = "phases"

  private lazy val FlagNames : Set[String] = allFlags.map(_.name)
  private lazy val OtherNames: Set[String] = Set(Phases)
  private lazy val ValidArguments          = FlagNames ++ OtherNames

  override def isValidArg(arg: String): Boolean = arg in ValidArguments

  override def description(implicit formatter: Formatter): String =
    s"""
       |Prints help information and exits. Giving a flag as argument will give more information about that flag.
       |${ flag(this) } ${highlight("phases")} prints information about the different phases of the T-Compiler.
      """

  override def parseValue(args: Set[String]): Set[String] = args

}
