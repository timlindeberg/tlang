package tlang.testutils.matcher

import org.scalatest.matchers.{MatchResult, Matcher}

object SymbolMatchers extends SymbolMatchers

trait SymbolMatchers {

  def haveSymbol: Matcher[Symbolic[_]] = (symbolic: Symbolic[_]) => MatchResult(
    symbolic.hasSymbol,
    s"$symbolic does not have a symbol",
    s"$symbolic has a symbol"
  )
}
