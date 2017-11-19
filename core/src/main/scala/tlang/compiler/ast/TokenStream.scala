package tlang.compiler.ast

import tlang.compiler.lexer.Tokens._
import tlang.compiler.lexer.{Token, TokenKind}
import tlang.utils.Extensions._

case class TokenStream(tokenList: Traversable[Token]) {

  private var currentIndex: Int = 0

  private val InvisibleTokens = List(NEWLINE, INDENT, DEDENT)

  // Remove comments and adjacent new line tokens
  private var tokens: Array[Token] = tokenList.filter(!_.isInstanceOf[COMMENTLIT]).toArray
  tokens = tokens
    .zipWithIndex
    .filter { case (token, i) => !(token.kind == NEWLINE && tokens(i + 1).kind == NEWLINE) }
    .map(_._1)

  var current: Token = tokens(currentIndex)
  val last   : Token = tokens.last

  def next: Token = if (current.kind == NEWLINE) tokens(currentIndex + 1) else current
  def nextKind: TokenKind = next.kind

  def lastVisible: Token = {
    if (currentIndex == 0)
      return current

    var i = currentIndex - 1
    while (i > 0 && (tokens(i).kind in InvisibleTokens))
      i -= 1

    tokens(i)
  }

  def apply(i: Int) = tokens(i)
  def offset(i: Int) = tokens(currentIndex + i)

  def readNext(): Unit = {
    currentIndex += 1
    if (currentIndex < tokens.length)
      current = tokens(currentIndex)
  }

  def readNewLines(): Unit =
    while (current.kind == NEWLINE)
      readNext()

  override def toString: String = tokens.drop(currentIndex).mkString(", ")


}
