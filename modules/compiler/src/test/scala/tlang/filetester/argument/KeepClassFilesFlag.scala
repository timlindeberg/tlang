package tlang
package filetester
package argument

import tlang.formatting.Formatter
import tlang.options.BooleanFlag

case object KeepGeneratedFilesFlag extends BooleanFlag {
  override val name = "keepgeneratedfiles"

  override def description(implicit formatter: Formatter): String =
    s"Keeps all generated .class files after compilation."
}
