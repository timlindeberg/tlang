package tlang.utils.formatting

import tlang.compiler.ast.{PrettyPrinter, TreePrinter}
import tlang.compiler.error.VoidReporter
import tlang.compiler.lexer.Lexer

object Formatter {
  def apply(formatting: Formatting): Formatter = apply(
    formatting,
    WordWrapper(),
    Truncator(),
    PrettyPrinter(formatting),
    TreePrinter(formatting),
    SyntaxHighlighter(Lexer(VoidReporter(), formatting), formatting),
    StackTraceHighlighter(formatting)
  )
}

case class Formatter(
  formatting: Formatting,
  wordWrapper: WordWrapper,
  truncator: Truncator,
  prettyPrinter: PrettyPrinter,
  treePrinter: TreePrinter,
  syntaxHighlighter: SyntaxHighlighter,
  stackTraceHighlighter: StackTraceHighlighter
) {

  def useColor: Boolean = formatting.useColor

}
