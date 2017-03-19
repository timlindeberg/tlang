package tlang.compiler.error

import tlang.compiler.Context
import tlang.compiler.lexer.Tokens._
import tlang.compiler.lexer.{Token, Tokenizer, Tokens}
import tlang.utils.Colors._
import tlang.utils.Extensions._
import tlang.utils._

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

    var line = 1
    var col = 1

    def updatePos(char: Char) = char match {
      case '\n' =>
        line += 1
        col = 1
      case _    => col += 1
    }

    val highlighted = code.toColoredString(colors) map { case cc@ColoredCharacter(color, char) =>
      val pos = Position(line, col, line, col + 1)

      updatePos(char)

      findMatchingMarking(pos, markings) match {
        case Some(style)              => ColoredCharacter(style, char)
        case None if color == NoColor => ColoredCharacter(getColor(pos, tokens.find(pos.isWithin(_))), char)
        case _                        => cc
      }
    }
    highlighted.toAnsiString
  }

  private def getColor(pos: Position, token: Option[Token]): Color = token match {
    case Some(x) => x.kind match {
      case NEWLINE                                                 => NoColor
      case COMMENTLITKIND                                          => CommentColor
      case INTLITKIND | LONGLITKIND | FLOATLITKIND | DOUBLELITKIND => NumColor
      case CHARLITKIND | STRLITKIND                                => StringColor
      case IDKIND                                                  => VarColor
      case x if x in Tokens.Keywords                               => KeywordColor
      case _                                                       => SymbolColor
    }
    case None    => NoColor
  }

  private def findMatchingMarking(pos: Position, markings: Seq[Marking]): Option[Color] =
    markings
      .find { case Marking(markedPos, _, offset) =>
        val offsetPos = Position(pos.line + offset - 1, pos.col, pos.endLine + offset - 1, pos.endCol)
        offsetPos.isWithin(markedPos)
      }
      .map(_.style)
}
