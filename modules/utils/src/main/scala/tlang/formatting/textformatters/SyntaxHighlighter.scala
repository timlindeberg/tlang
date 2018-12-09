package tlang
package formatting
package textformatters

import tlang.formatting.Colors._
import tlang.formatting.Formatter
import tlang.utils.{Position, Positioned}


case class Coloring(color: Color, position: Positioned)
case class Marking(pos: Positioned, style: Color, isAdditive: Boolean = false, lineOffset: Int = 1)

case class SyntaxHighlighter(coloring: String => Seq[Coloring])(implicit formatter: Formatter) {

  import formatter._

  def apply(code: String): String = apply(code, Seq())
  def apply(code: String, marking: Marking): String = apply(code, Seq(marking))
  def apply(code: String, markings: Seq[Marking] = Seq()): String = {
    if (code.isEmpty || !useColor)
      return code

    val (codeWithoutColors, colors) = splitStringAndColors(code)

    val colorings = coloring(codeWithoutColors)
    if (colorings.isEmpty)
      return code


    calculateColors(colors, codeWithoutColors, colorings.iterator, markings)
    constructColoredString(codeWithoutColors, colors)
  }

  // Updates the color values in the colors array with syntax highlighting and markings
  private def calculateColors(colors: Array[Color], code: String, colorings: Iterator[Coloring], markings: Seq[Marking]): Unit = {
    var line = 1
    var col = 1

    var coloring = colorings.next()
    var color = coloring.color
    var nextColoring: Option[Coloring] = if (colorings.hasNext) Some(colorings.next) else None

    for (i <- 0 until code.length) {
      val pos = Position(line, col, line, col + 1)
      val encodedStart = pos.encodedStartPos
      while (nextColoring.isDefined && encodedStart >= nextColoring.get.position.encodedStartPos) {
        coloring = nextColoring.get
        color = coloring.color
        nextColoring = if (colorings.hasNext) Some(colorings.next) else None
      }

      code(i) match {
        case '\n' =>
          line += 1
          col = 1
        case _    => col += 1
      }


      colors(i) = getColor(colors(i), color, pos, markings)
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

    def addColor(color: Color): Unit = {
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

  private def findMatchingMarking(pos: Position, markings: Seq[Marking]): Option[Marking] =
    markings.find { case Marking(markedPos, _, _, offset) =>
      val offsetPos = Position(pos.line + offset - 1, pos.col, pos.lineEnd + offset - 1, pos.colEnd)
      offsetPos isWithin markedPos
    }
}
