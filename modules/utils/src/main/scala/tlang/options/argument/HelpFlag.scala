package tlang
package options
package argument

import tlang.formatting.Formatter

// Create a global HelpFlag object with no valid flags to be able
// to access name, etc. in other flags
object HelpFlag extends HelpFlag(Set())

// All flags is a function so we can pass the list of compiler flags to the help flag.
// Otherwise we get a null pointer exception since the Help Flag is part of the compiler flags
// list and gets initialized before the list exists.
class HelpFlag(allFlags: => Set[FlagArgument[_]]) extends OptionalArgumentFlag[Set[String]] {
  override val name: String = "help"
  override val shortFlag: Some[String] = Some("h")
  override val argDescription: String = "about"
  override val defaultArg: String = "all"

  val Phases: String = "phases"

  private lazy val FlagNames: Set[String] = allFlags.map(_.name)
  private lazy val OtherNames: Set[String] = Set(Phases)
  private lazy val ValidArguments = FlagNames ++ OtherNames

  override def isValidArg(arg: String): Boolean = arg in ValidArguments

  override def description(implicit formatter: Formatter): String =
    s"""
       |Prints help information and exits. Giving a flag as argument will give more information about that flag.
       |$formattedName ${ highlight(Phases) } prints information about the different phases of the T-Compiler.
      """

  override def parseValue(args: Set[String]): Set[String] = args
}
