package tlang
package filetester
package argument

import tlang.formatting.Formatter
import tlang.options.BooleanFlag

case object KeepClassFilesFlag extends BooleanFlag {
  override val name = "keepclassfiles"

  override def description(implicit formatter: Formatter): String =
    s"Keeps all generated .class files after compilation."
}
