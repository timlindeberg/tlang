package tlang
package compiler
package ast

import tlang.compiler.lexer.Token
import tlang.compiler.lexer.Tokens._
import tlang.utils.{Position, Source}

import scala.collection.mutable.ListBuffer

case class TokenStream(tokenList: Seq[Token]) {

  private var currentIndex: Int = 0
  private var leftOverGreaterThanFromRShift: Option[Token] = None

  private val InvisibleTokens = List(NEWLINE, INDENT, DEDENT)

  // Remove comments and adjacent new line tokens
  private val tokens: ListBuffer[Token] = filterTokenList(tokenList)

  private var _nextIncludingNewlines: Token = tokens(0)
  private var _nextToken: Token = calculateNext()

  def nextIncludingNewlines: Token = leftOverGreaterThanFromRShift match {
    case Some(token) => token
    case None        => _nextIncludingNewlines
  }

  def next: Token = leftOverGreaterThanFromRShift match {
    case Some(token) => token
    case None        => _nextToken
  }

  val last: Token = tokens.last
  var lastVisible: Token = nextIncludingNewlines

  def source: Option[Source] = last.source
  def apply(i: Int): Token = tokens(i)
  def offset(i: Int): Token = tokens(currentIndex + i)

  def readNext(): Unit = {
    if (leftOverGreaterThanFromRShift.isDefined) {
      leftOverGreaterThanFromRShift = None
      return
    }

    currentIndex += 1
    if (currentIndex < tokens.length)
      _nextIncludingNewlines = tokens(currentIndex)

    lastVisible = calculateLastVisible()
    _nextToken = calculateNext()
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
   * treating RSHIFT (>>) as two ">". Real hack.
   */
  def useRShiftAsGreaterThan(): Unit = {
    val pos = Position(lastVisible)
    pos.col += 1
    val token = new Token(GREATERTHAN).setPos(pos)
    leftOverGreaterThanFromRShift = Some(token)
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
    tokenList
      .filter { !_.isInstanceOf[COMMENTLIT] }
      .zipWithIndex
      .filter { case (token, i) => !(token.kind == NEWLINE && i < tokenList.size - 1 && tokenList(i + 1).kind == NEWLINE) }
      .foreach { elem => buff += elem._1 }
    buff
  }
}
