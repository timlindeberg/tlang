package tlang.formatting.textformatters

import tlang.compiler.lexer.Tokens._
import tlang.compiler.lexer.{Lexer, Token}
import tlang.formatting.Colors._
import tlang.formatting.Formatting
import tlang.utils.Extensions._
import tlang.utils.{Position, Positioned, StringSource}

case class Marking(pos: Positioned, style: Color, isAdditive: Boolean = false, lineOffset: Int = 1)

case class SyntaxHighlighter(lexer: Lexer, formatting: Formatting) {

  import formatting._

  def apply(code: String): String = apply(code, Seq())
  def apply(code: String, marking: Marking): String = apply(code, Seq(marking))
  def apply(code: String, markings: Seq[Marking] = Seq()): String = {
    if (code.isEmpty || !formatting.useColor)
      return code

    val (codeWithoutColors, colors) = splitStringAndColors(code)

    val tokens = lexer(StringSource(codeWithoutColors, ""))

    calculateColors(colors, codeWithoutColors, tokens.iterator, markings)
    constructColoredString(codeWithoutColors, colors)
  }

  // Updates the color values in the colors array with syntax highlighting and markings
  private def calculateColors(colors: Array[Color], code: String, tokens: Iterator[Token], markings: Seq[Marking]): Unit = {
    var line = 1
    var col = 1

    var token = tokens.next()
    var tokenColor = getColor(token)
    var nextToken: Option[Token] = if (tokens.hasNext) Some(tokens.next) else None

    for (i <- 0 until code.length) {
      val pos = Position(line, col, line, col + 1)
      val encodedStart = pos.encodedStartPos
      while (nextToken.isDefined && encodedStart >= nextToken.get.encodedStartPos) {
        token = nextToken.get
        tokenColor = getColor(token)
        nextToken = if (tokens.hasNext) Some(tokens.next) else None
      }

      code(i) match {
        case '\n' =>
          line += 1
          col = 1
        case _    => col += 1
      }


      colors(i) = getColor(colors(i), tokenColor, pos, markings)
    }
  }

  // Markings has the highest precedence, then comes the color that was already
  // in the string, after that the corresponding token color.
  // If the marking is additive it is combined with either the existing color if any or
  // the token color
  private def getColor(existingColor: Color, tokenColor: Color, pos: Position, markings: Seq[Marking]): Color = {
    findMatchingMarking(pos, markings) match {
      case Some(Marking(_, markingColor, isAdditive, _)) =>
        if (!isAdditive)
          return markingColor

        if (existingColor != NoColor) existingColor + markingColor else tokenColor + markingColor
      case None if existingColor != NoColor              => existingColor
      case _                                             => tokenColor
    }
  }

  private def constructColoredString(code: String, colors: IndexedSeq[Color]) = {
    val sb = new StringBuilder(code.length)
    var previousColor: Color = NoColor
    var i = 0

    def addColor(color: Color) = {
      if (color != previousColor) {
        if (previousColor needsResetBefore color)
          sb ++= Reset
        sb ++= color
        previousColor = color
      }
    }

    while (i < code.length) {
      val color = colors(i)
      code(i) match {
        case '\r' if i + 1 < code.length && code(i + 1) == '\n' =>
          sb += '\r'
          i += 1
        case '\n'                                               =>
        case _                                                  => addColor(color)
      }
      sb += code(i)
      i += 1
    }
    if (previousColor != NoColor)
      sb ++= Reset
    sb.toString()
  }

  private def getColor(token: Token): Color = token.kind match {
    case NEWLINE | INDENT | DEDENT | BAD                         => NoColor
    case COMMENTLITKIND                                          => CommentColor
    case INTLITKIND | LONGLITKIND | FLOATLITKIND | DOUBLELITKIND => NumColor
    case CHARLITKIND | STRLITKIND                                => StringColor
    case IDKIND                                                  => VarColor
    case x if x in Keywords                                      => KeywordColor
    case _                                                       => SymbolColor
  }

  private def findMatchingMarking(pos: Position, markings: Seq[Marking]): Option[Marking] =
    markings.find { case Marking(markedPos, _, _, offset) =>
      val offsetPos = Position(pos.line + offset - 1, pos.col, pos.endLine + offset - 1, pos.endCol)
      offsetPos isWithin markedPos
    }
}
