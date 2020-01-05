package tlang
package compiler
package ast

import tlang.compiler.lexer.Token
import tlang.compiler.lexer.Tokens._
import tlang.utils.{Position, Source}

import scala.collection.mutable.ListBuffer

case class TokenStream(tokenList: Seq[Token]) {

  private var currentIndex: Int = 0
  private val InvisibleTokens = List(NEWLINE, INDENT, DEDENT)

  private val tokens: ListBuffer[Token] = filterTokenList(tokenList)

  var nextIncludingNewlines: Token = tokens.head
  var next: Token = calculateNext()

  val last: Token = tokens.last
  var lastVisible: Token = nextIncludingNewlines

  def source: Option[Source] = last.source
  def apply(i: Int): Token = tokens(i)
  def offset(i: Int): Token = tokens(currentIndex + i)

  def readNext(): Unit = {
    currentIndex += 1
    if (currentIndex < tokens.length)
      nextIncludingNewlines = tokens(currentIndex)

    lastVisible = calculateLastVisible()
    next = calculateNext()
  }

  def readNewLines(): Int = {
    var num = 0
    while (nextIncludingNewlines.kind == NEWLINE) {
      readNext()
      num += 1
    }
    num
  }

  /**
   * Handles generics having multiple ">" signs by
   * replacing an RSHIFT (>>) with two ">".
   */
  def useRightShiftAsTwoGreaterThan(): Unit = {
    assert(next.kind == RSHIFT)

    val firstPos = Position(next)
    firstPos.colEnd -= 1
    val firstToken = new Token(GREATERTHAN).setPos(firstPos)

    val secondPos = Position(next)
    secondPos.col += 1
    val secondToken = new Token(GREATERTHAN).setPos(secondPos)

    tokens.remove(currentIndex)
    tokens.insert(currentIndex, firstToken, secondToken)
    nextIncludingNewlines = firstToken
    next = firstToken
  }

  override def toString: String = {
    val maxTokensToShow = 20
    if (tokens.length - currentIndex <= maxTokensToShow)
      tokens.drop(currentIndex).mkString(" ")
    else
      tokens.slice(currentIndex, currentIndex + maxTokensToShow).mkString(" ") + " ..."
  }

  private def calculateLastVisible(): Token = {
    if (currentIndex == 0)
      return nextIncludingNewlines

    var i = currentIndex - 1
    while (i > 0 && (tokens(i).kind in InvisibleTokens))
      i -= 1

    tokens(i)
  }

  private def calculateNext(): Token = if (nextIncludingNewlines.kind == NEWLINE) tokens(currentIndex + 1) else nextIncludingNewlines

  private def filterTokenList(tokenList: Seq[Token]): ListBuffer[Token] = {
    val buff = ListBuffer[Token]()
    val tokens: Seq[Token] = tokenList.filter { !_.isInstanceOf[COMMENTLIT] }
    buff ++= tokens
      .zipWithIndex
      .filter { case (token, i) => !(token.kind == NEWLINE && tokens(i + 1).kind == NEWLINE) }
      .map(_._1)
    buff
  }
}
