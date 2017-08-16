package tlang.utils.formatting

import tlang.utils.Extensions._

case class Truncator() {

  private val TruncationChar : Char   = '.'
  private val TruncationWidth: Int    = 3
  private val Truncation     : String = s"$TruncationChar" * TruncationWidth

  def apply(line: String, width: Int): String = {
    if (width < 0)
      throw new IllegalArgumentException(s"Width can not be 0 or smaller: $width")

    val lineWidth = line.charCount
    if (lineWidth <= width)
      return line

    if (width == 0)
      return ""

    if (width <= TruncationWidth)
      return s"$TruncationChar" * width

    var resetAnsi = false
    var i = 0
    var len = 0
    while (i < line.length) {
      if (len == width - TruncationWidth) {
        val truncated = line.substring(0, i)
        if (resetAnsi)
          return truncated + Console.RESET + Truncation
        return truncated + Truncation
      }

      line(i) match {
        case '\u001b' if line(i + 1) == '[' =>
          val endOfAnsi = line.indexOf('m', i + 1)
          val ansi = line.substring(i, endOfAnsi + 1)
          resetAnsi = ansi != Console.RESET
          i = endOfAnsi
        case _                              =>
          len += 1
      }
      i += 1
    }
    line
  }
}
