package tlang.options.arguments

import tlang.compiler.Main
import tlang.formatting.Formatter
import tlang.options.OptionalArgumentFlag
import tlang.utils.Extensions._

case object HelpFlag extends OptionalArgumentFlag[Set[String]] {
  override val name           = "help"
  override val shortFlag      = Some("h")
  override val argDescription = "about"
  override val defaultArg     = "all"

  private lazy val FlagNames = Main.CompilerFlags.map(_.name)

  override def isValidArg(arg: String): Boolean = arg in FlagNames

  override def description(formatter: Formatter): String = {
    s"""
       |Prints help information and exits. Giving a flag as argument will give more information about that flag.
      """.stripMargin.trim
  }

  override def parseValue(args: Set[String]): Set[String] = args

}