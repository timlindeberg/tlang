package tlang
package compiler
package argument

import tlang.formatting.Formatter
import tlang.options.BooleanFlag


case object WatchFlag extends BooleanFlag {
  override val name = "watch"

  override def description(implicit formatter: Formatter): String =
    "Watches for changes to the given files and reruns the compiler when a change is found."

}
