package tlang.formatting.grid


trait Width
trait FixedWidth extends Width {
  def apply(maxWidth: Int): Int
}

object Width {
  case object Auto extends Width

  case class Fixed(width: Int) extends FixedWidth {
    def apply(maxWidth: Int): Int = width
  }

  case class Percentage(widthPercentage: Double) extends FixedWidth {
    if (widthPercentage < 0.0 || widthPercentage > 1.0)
      throw new IllegalArgumentException("Percentage width should be between 0 and 1")

    def apply(maxWidth: Int): Int = (widthPercentage * maxWidth).toInt
  }
}
