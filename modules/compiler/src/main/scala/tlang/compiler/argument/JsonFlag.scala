package tlang
package compiler
package argument

import tlang.formatting.Formatter
import tlang.options.{Argument, BooleanFlag}

case object JSONFlag extends BooleanFlag {
  override val name = "json"

  override def description(implicit formatter: Formatter): String =
    "Outputs all compiler messages and information in JSON."

  override def extendedDescription(implicit formatter: Formatter): String =
    s"""
       |Outputs all compiler messages and information in JSON.
       |
       |All output of the compiler will be delayed until the end of compilation where exactly one JSON object will be output.
       |This JSON object will always contain a key ${ highlight("success") } with a boolean indicating whether execution was successful or not.
       |Compilation errors and warnings will be under the keys ${ highlight("compilationErrors") } and ${ highlight("compilationWarnings") }.
       |
       |Adding the flag ${ VerboseFlag.formattedName } will output execution times of the different compiler phases under the key ${ highlight("executionTimes") } as well as various other output.
       |
       |Running the compiler with the ${ ExecFlag.formattedName } will output execution information under the key ${ highlight("execution") }.
       |
       |Cannot be used in combination with ${ WatchFlag.formattedName }
      """

  override def incompatibleWith: Set[Argument[_]] = Set(WatchFlag)
}
