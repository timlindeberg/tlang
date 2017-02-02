package tcompiler.error

import tcompiler.lexer.Tokens._
import tcompiler.lexer.{Token, Tokenizer, Tokens}
import tcompiler.utils.Extensions._
import tcompiler.utils.{Colorizer, Context, Positioned}

/**
  * Created by Tim Lindeberg on 1/29/2017.
  */

case class Marking(lineOffset: Int, pos: Positioned, style: String)

class SyntaxHighlighter(colorizer: Colorizer) {

  import colorizer._

  val context = Context(new VoidReporter(), Nil)

  def apply(code: String, markings: Marking*): String = {
    if (!colorizer.useColor)
      return code

    val tokenizer = new Tokenizer(context, None)
    val tokens = tokenizer.apply(code.toList)
    val sb = new StringBuilder()
    sb ++= code.substring(0, tokens.head.col - 1)
    var prevColor = ""

    for (token :: next :: Nil <- tokens.sliding(2)) {
      val start = token.col - 1
      val end = token.endCol - 1

      val color = getColor(token, markings)
      if (color != prevColor) {
        if (prevColor != "")
          sb ++= Reset
        sb ++= color
      }
      prevColor = color
      sb ++= code.substring(start, end)
      if (next.kind != EOF) {
        val nextColor = getColor(next, markings)
        if (nextColor != color)
          sb ++= Reset

        sb ++= code.substring(end, next.col - 1)
      }
    }

    if (tokens.length >= 2) {
      val last = tokens(tokens.length - 2)
      sb ++= code.substring(last.endCol - 1, code.length)
    }
    sb.toString()
  }


  def getColor(token: Token, markings: Seq[Marking]): String = {
    if (token.kind == COMMENTLITKIND)
      return CommentColor

    findMatchingMarking(token, markings) match {
      case Some(style) => style
      case None        => token.kind match {
        case INTLITKIND | LONGLITKIND | FLOATLITKIND | DOUBLELITKIND => NumColor
        case CHARLITKIND | STRLITKIND                                => StringColor
        case IDKIND                                                  => VarColor
        case x if x in Tokens.Keywords                               => KeywordColor
        case _                                                       => ""
      }
    }
  }

  private def findMatchingMarking(token: Token, markings: Seq[Marking]): Option[String] =
    markings
      .find { case Marking(offset, pos, _) =>
        encodePos(token.line + offset - 1, token.col) >= encodePos(pos.line, pos.col) &&
          encodePos(token.endLine + offset - 1, token.endCol) <= encodePos(pos.endLine, pos.endCol)
      }
      .map(_.style)

  private def encodePos(line: Int, col: Int) = (line << 16) + col

}
