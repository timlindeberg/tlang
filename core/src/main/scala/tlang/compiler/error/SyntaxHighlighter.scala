package tlang.compiler.error

import tlang.compiler.Context
import tlang.compiler.lexer.Tokens._
import tlang.compiler.lexer.{Token, Tokenizer, Tokens}
import tlang.utils.Colors._
import tlang.utils.Extensions._
import tlang.utils.{Colors, Position, Positioned, StringSource}

/**
  * Created by Tim Lindeberg on 1/29/2017.
  */

case class Marking(pos: Positioned, style: Color, lineOffset: Int = 1)

case class SyntaxHighlighter(colors: Colors) {

  import colors._

  val context = Context(VoidReporter(), Set())

  def apply(code: String, marking: Marking): String = apply(code, Seq(marking))
  def apply(code: String, markings: Seq[Marking] = Seq()): String = {
    if (code.isEmpty || !colors.isActive)
      return code

    val source = StringSource(code, "")
    val tokenizer = new Tokenizer(context, source)
    val tokens = tokenizer()
    val sb = new StringBuilder()

    val lines = code.split("\n", -1)
    sb ++= code.substring(0, tokens.head.col - 1)
    val noColor = Color("", isActive = true)
    var prevColor = noColor

    var lineIndex = 0
    for (token :: next :: Nil <- tokens.sliding(2)) {

      val line = lines(lineIndex)

      val start = token.col - 1
      val end = token.endCol - 1

      val color = getColor(token, markings)
      if (color != prevColor) {
        if (prevColor != noColor)
          sb ++= Reset
        sb ++= color
      }

      prevColor = color
      token.kind match {
        case NEWLINE =>
          lineIndex += 1
          sb += '\n'
          sb ++= Reset
          if (lineIndex < lines.length && next.kind != EOF)
            sb ++= lines(lineIndex).substring(0, next.col - 1)
          sb ++= color
        case EOF     => sb ++= line.substring(start, end)
        case _       =>
          sb ++= line.substring(start, end)
          if (next.kind != EOF) {
            val nextColor = getColor(next, markings)
            if (nextColor != color)
              sb ++= Reset

            sb ++= line.substring(end, next.col - 1)
          }
      }
    }

    val lastLine = lines.last
    if (lastLine.nonEmpty && tokens.length >= 2) {
      val lastToken = tokens(tokens.length - 2)
      if (lastToken.endLine < lines.size)
        sb ++= lastLine
      else
        sb ++= lastLine.substring(lastToken.endCol - 1, lastLine.length)
    }


    sb ++= Reset
    sb.toString
  }

  private def getColor(token: Token, markings: Seq[Marking]): Color = {
    if (token.kind == COMMENTLITKIND)
      return CommentColor

    findMatchingMarking(token, markings) ifDefined { style =>
      return style
    }

    token.kind match {
      case INTLITKIND | LONGLITKIND | FLOATLITKIND | DOUBLELITKIND => NumColor
      case CHARLITKIND | STRLITKIND                                => StringColor
      case IDKIND                                                  => VarColor
      case x if x in Tokens.Keywords                               => KeywordColor
      case _                                                       => SymbolColor
    }
  }

  private def findMatchingMarking(token: Token, markings: Seq[Marking]): Option[Color] =
    markings
      .find { case Marking(pos, _, offset) =>
        val offsetPos = Position(token.line + offset - 1, token.col, token.endLine + offset - 1, token.endCol)
        offsetPos.encodedStartPos >= pos.encodedStartPos && offsetPos.encodedEndPos <= pos.encodedEndPos
      }
      .map(_.style)
}
