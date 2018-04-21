package tlang.formatting.grid

import tlang.utils.Extensions._

trait Alignment {
  def apply(text: String, width: Int, fill: String = " "): String = {
    if (width < 1)
      throw new IllegalArgumentException(s"Cannot align text within a space smaller than 1: $width")

    val textWidth = text.visibleCharacters
    if (textWidth > width)
      throw new IllegalArgumentException(s"Cannot align text '$text' in the given space: $textWidth > $width")

    align(text, width - textWidth, fill)
  }

  protected def align(text: String, space: Int, fill: String): String
}

object Alignment {
  case object Left extends Alignment {
    override def align(text: String, space: Int, fill: String): String = {
      text + fill * space
    }
  }

  case object Right extends Alignment {
    override def align(text: String, space: Int, fill: String): String = {
      fill * space + text
    }
  }

  case object Center extends Alignment {
    override def align(text: String, space: Int, fill: String): String = {
      val halfSpace = fill * (space / 2)
      val left = halfSpace
      val right = if (space % 2 == 0) halfSpace else halfSpace + fill
      left + text + right
    }
  }
}
