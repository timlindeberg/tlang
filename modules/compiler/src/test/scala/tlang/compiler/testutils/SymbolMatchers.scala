package tlang
package compiler
package testutils

import org.scalatest.matchers.{MatchResult, Matcher}
import tlang.compiler.analyzer.Symbols.Symbolic

object SymbolMatchers extends SymbolMatchers

trait SymbolMatchers {

  def haveSymbol: Matcher[Symbolic[_]] = (symbolic: Symbolic[_]) => MatchResult(
    symbolic.hasSymbol,
    s"$symbolic does not have a symbol",
    s"$symbolic has a symbol"
  )

}
