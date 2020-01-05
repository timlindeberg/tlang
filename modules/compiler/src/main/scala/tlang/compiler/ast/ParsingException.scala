package tlang.compiler.ast

import sourcecode.{Enclosing, Line}
import tlang.compiler.lexer.{Token, TokenKind}

object ParsingException {
  def message(wrongToken: Token, expected: Seq[TokenKind]) = s"Found '$wrongToken'${ format(expected) }"
  private def format(expected: Seq[TokenKind]) =
    if (expected.isEmpty) "" else s", expected ${ formatTokens(expected) }"

  private def formatTokens(tokens: Seq[TokenKind]) = tokens
    .map { t => s"'$t'" }
    .mkString(" or ")
}

case class ParsingException(wrongToken: Token, expected: Seq[TokenKind])
  (implicit val enclosing: Enclosing, val line: Line)
  extends Exception(ParsingException.message(wrongToken, expected))
