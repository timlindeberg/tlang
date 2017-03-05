package tlang.compiler.error

import tlang.compiler.Context
import tlang.compiler.lexer.Tokens._
import tlang.compiler.lexer.{Token, Tokenizer, Tokens}
import tlang.compiler.utils._
import tlang.utils.Colors._
import tlang.utils.Extensions._
import tlang.utils.{Colors, StringSource}

/**
  * Created by Tim Lindeberg on 1/29/2017.
  */

case class Marking(lineOffset: Int, pos: Positioned, style: Color)

case class SyntaxHighlighter(colors: Colors) {

  import colors._

  val context = Context(VoidReporter(), Set())

  def apply(code: String, markings: Seq[Marking] = Seq()): String = {
    if (!colors.isActive)
      return code
    val lines = code.split("\n", -1)
    val highlighted = lines.map(highlight(_, markings))
    highlighted.mkString("\n")
  }


  private def highlight(line: String, markings: Seq[Marking]): String = {
    if (line.isEmpty)
      return line

    val source = StringSource(line, "")
    val tokenizer = new Tokenizer(context, source)
    val tokens = tokenizer()
    val sb = new StringBuilder()
    sb ++= line.substring(0, tokens.head.col - 1)
    val noColor = Color("", isActive = true)
    var prevColor = noColor

    for (token :: next :: Nil <- tokens.sliding(2)) {
      val start = token.col - 1
      val end = token.endCol - 1

      val color = getColor(token, markings)
      if (color != prevColor) {
        if (prevColor != noColor)
          sb ++= Reset
        sb ++= color
      }
      prevColor = color
      sb ++= line.substring(start, end)
      if (next.kind != EOF) {
        val nextColor = getColor(next, markings)
        if (nextColor != color)
          sb ++= Reset

        sb ++= line.substring(end, next.col - 1)
      }
    }

    if (tokens.length >= 2) {
      val last = tokens(tokens.length - 2)
      sb ++= line.substring(last.endCol - 1, line.length)
    }
    sb.toString()
  }

  def getColor(token: Token, markings: Seq[Marking]): Color = {
    if (token.kind == COMMENTLITKIND)
      return CommentColor

    findMatchingMarking(token, markings) match {
      case Some(style) => style
      case None        => token.kind match {
        case INTLITKIND | LONGLITKIND | FLOATLITKIND | DOUBLELITKIND => NumColor
        case CHARLITKIND | STRLITKIND                                => StringColor
        case IDKIND                                                  => VarColor
        case x if x in Tokens.Keywords                               => KeywordColor
        case _                                                       => SymbolColor
      }
    }
  }

  private def findMatchingMarking(token: Token, markings: Seq[Marking]): Option[Color] =
    markings
      .find { case Marking(offset, pos, _) =>
        val offsetPos = Position(token.line + offset - 1, token.col, token.endLine + offset - 1, token.endCol)
        offsetPos.encodedStartPos >= pos.encodedStartPos && offsetPos.encodedEndPos <= pos.encodedEndPos
      }
      .map(_.style)
}
