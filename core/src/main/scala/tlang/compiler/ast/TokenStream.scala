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

  var current    : Token = tokens(currentIndex)
  val last       : Token = tokens.last
  var lastVisible: Token = current

  def next: Token = if (current.kind == NEWLINE) tokens(currentIndex + 1) else current
  def nextKind: TokenKind = next.kind

  def apply(i: Int) = tokens(i)
  def offset(i: Int) = tokens(currentIndex + i)

  def readNext(): Unit = {
    currentIndex += 1
    if (currentIndex < tokens.length)
      current = tokens(currentIndex)
    lastVisible = calculateLastVisible
  }

  def readNewLines(): Int = {
    var num = 0
    while (current.kind == NEWLINE) {
      readNext()
      num += 1
    }
    num
  }


  override def toString: String = {
    val maxTokensToShow = 20
    if (tokens.length - currentIndex <= maxTokensToShow)
      tokens.drop(currentIndex).mkString(" ")
    else
      tokens.slice(currentIndex, currentIndex + maxTokensToShow).mkString(" ") + " ..."
  }


  private def calculateLastVisible: Token = {
    if (currentIndex == 0)
      return current

    var i = currentIndex - 1
    while (i > 0 && (tokens(i).kind in InvisibleTokens))
      i -= 1

    tokens(i)
  }

}
