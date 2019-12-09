package tlang
package formatting
package grid

trait OverflowHandling {
  def apply(line: String, width: Int)(implicit formatter: Formatter): List[String] = {
    if (line.isEmpty)
      return List("")

    handleOverflow(line, width)
  }

  def handleOverflow(line: String, width: Int)(implicit formatter: Formatter): List[String]
}

object OverflowHandling {
  case object Except extends OverflowHandling {
    override def handleOverflow(line: String, width: Int)(implicit formatter: Formatter): List[String] = {
      val lineWidth = line.visibleCharacters
      if (lineWidth > width)
        throw new IllegalStateException(s"Cannot fit line $line in the given space: $lineWidth > $width")
      formatter.splitWithColors(line)
    }
  }

  case object Wrap extends OverflowHandling {
    override def handleOverflow(line: String, width: Int)(implicit formatter: Formatter): List[String] = {
      formatter.wordWrap(line, width)
    }
  }

  case object Truncate extends OverflowHandling {
    override def handleOverflow(line: String, width: Int)(implicit formatter: Formatter): List[String] = {
      val lines = formatter.splitWithColors(line)
      lines map { formatter.truncate(_, width) }
    }
  }
}
