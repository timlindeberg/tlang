package tlang.compiler.output

import tlang.compiler.ast.{PrettyPrinter, TreePrinter}
import tlang.formatting.Formatter
import tlang.formatting.textformatters.TabReplacer
import tlang.utils.Factory

case class OutputFactory(formatter: Formatter, prettyPrinter: PrettyPrinter, tabReplacer: TabReplacer, treePrinter: TreePrinter)
  extends Factory[Output](formatter, prettyPrinter, tabReplacer, treePrinter)
