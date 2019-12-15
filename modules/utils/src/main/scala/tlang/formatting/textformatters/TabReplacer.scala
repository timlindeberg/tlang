package tlang
package formatting
package textformatters

import tlang.utils.{Position, Positioned}

case class TabReplacer(tabWidth: Int) {

  private val TabReplacement = " " * tabWidth

  def apply(text: String): String = text.replaceAll("\t", " " * tabWidth)

  def apply(text: String, pos: Positioned): (String, Positioned) = {
    val (t, p) = apply(text, Seq(pos))
    (t, p.head)
  }

  def apply(text: String, positionsToAlter: Seq[Positioned]): (String, Seq[Positioned]) = {
    val lines = text.split(NL, -1).zipWithIndex.map { case (s, i) => (s, i + 1) }
    val (adjustedText, adjustedPositions) = apply(lines, positionsToAlter)
    (adjustedText.map(_._1).mkString(NL), adjustedPositions)
  }

  def apply(lines: Seq[(String, Int)], pos: Positioned): (Seq[(String, Int)], Positioned) = {
    val (t, p) = apply(lines, Seq(pos))
    (t, p.head)
  }

  def apply(lines: Seq[(String, Int)], positionsToAlter: Seq[Positioned]): (Seq[(String, Int)], Seq[Positioned]) = {
    var positions = positionsToAlter

    val adjustedLines = lines map { case (line, lineNum) =>
      var sb = new StringBuilder
      for (i <- 0 until line.length) {
        val c = line(i)
        c match {
          case '\t' =>
            val length = sb.size
            positions = positions.map { pos =>
              var start = pos.col
              var end = pos.colEnd
              if (lineNum == pos.line && start - 1 > length) start += tabWidth - 1
              if (lineNum == pos.lineEnd && end - 1 > length) end += tabWidth - 1
              Position(pos.line, start, pos.lineEnd, end)
            }

            sb ++= TabReplacement
          case _    =>
            sb += c
        }
      }
      (sb.toString, lineNum)
    }

    (adjustedLines, positions)
  }
}
