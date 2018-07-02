package tlang.compiler.argument

import tlang.formatting.Formatter
import tlang.options.{BooleanFlag, FlagArgument}


case object JSONFlag extends BooleanFlag {
  override val name = "json"

  override def description(formatter: Formatter): String = {
    "Changes all output of the compiler to JSON output."
  }

  override def extendedDescription(formatter: Formatter): String = {
    import formatter.formatting._

    def key(key: String) = Blue(key)
    def flag(flag: FlagArgument[_]) = s"--${ Magenta(flag.name) }"

    s"""|Changes all output of the compiler to parsable JSON output.
        |
        |All output of the compiler will be delayed until the end of compilation where exactly one JSON object will be output.
        |This JSON object will always contain a key ${key("success")} with a boolean indicating whether execution was successful or not.
        |Compilation errors and warnings will be under the keys ${key("compilationErrors")} and ${key("compilationWarnings")}.
        |
        |Adding the flag ${flag(VerboseFlag) } will output execution times of the different compiler phases under the key ${key("executionTimes")}
        |as well as various other output.
        |
        |Running the compiler with the ${flag(ExecFlag)} will output execution information under the key ${key("execution")}.""".stripMargin
  }

}
