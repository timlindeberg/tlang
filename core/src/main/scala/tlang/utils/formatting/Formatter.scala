package tlang.utils.formatting

import tlang.compiler.ast.Trees.Tree
import tlang.compiler.ast.{PrettyPrinter, TreePrinter}
import tlang.compiler.error.VoidReporter
import tlang.compiler.lexer.Lexer

object Formatter {
  def apply(formatting: Formatting): Formatter = {
    apply(
      formatting,
      WordWrapper(),
      Truncator(),
      PrettyPrinter(formatting),
      TreePrinter(formatting),
      SyntaxHighlighter(Lexer(VoidReporter(), ErrorStringContext(formatting)), formatting),
      StackTraceHighlighter(formatting)
    )
  }
}

case class Formatter(
  formatting: Formatting,
  private val wordWrapper: WordWrapper,
  private val truncator: Truncator,
  private val prettyPrinter: PrettyPrinter,
  private val treePrinter: TreePrinter,
  private val syntaxHighlighter: SyntaxHighlighter,
  private val stackTraceHighlighter: StackTraceHighlighter
) {

  def useColor: Boolean = formatting.useColor


  def wrap(text: String, width: Int) = wordWrapper(text, width)
  def truncate(line: String, width: Int) = truncator(line, width)
  def prettyPrint(tree: Tree) = prettyPrinter(tree)
  def prettyPrint(trees: Traversable[Tree]) = prettyPrinter(trees)
  def formatTree(tree: Tree) = treePrinter(tree)
  def syntaxHighlight(code: String) = syntaxHighlighter(code, Seq())
  def syntaxHighlight(code: String, marking: Marking) = syntaxHighlighter(code, marking)
  def syntaxHighlight(code: String, markings: Seq[Marking]) = syntaxHighlighter(code, markings)
  def highlightStackTrace(throwable: Throwable) = stackTraceHighlighter(throwable)
  def highlightStackTrace(stackTrace: String) = stackTraceHighlighter(stackTrace)


}
