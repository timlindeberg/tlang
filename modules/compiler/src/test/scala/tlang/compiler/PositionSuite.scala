package tlang.compiler

import org.scalatest.Suites
import tlang.compiler.ast.ParsingPositionSpec
import tlang.compiler.lexer.LexingPositionSpec

class PositionSuite extends Suites(new LexingPositionSpec, new ParsingPositionSpec) {
  override def suiteName: String = "Positions"
}