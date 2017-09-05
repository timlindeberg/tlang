package tlang.compiler

import org.scalatest.Suites
import tlang.compiler.ast.ParsingPositionSpec
import tlang.compiler.lexer.LexingPositionSpec

class Positions extends Suites(new LexingPositionSpec, new ParsingPositionSpec)