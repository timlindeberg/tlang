package tlang.formatting

import scala.concurrent.duration.FiniteDuration

sealed abstract class Spinner(val frameTime: FiniteDuration, images: String*) {

  private var index        = 0
  private var _elapsedTime = FiniteDuration(0, "ms")

  def nextImage: String = {
    _elapsedTime += frameTime
    val image = images(index)
    index = (index + 1) % images.size
    image
  }

  def elapsedTime: FiniteDuration = _elapsedTime
  def reset(): Unit = {
    _elapsedTime = FiniteDuration(0, "ms")
    index = 0
  }
}

case class AwesomeSpinner() extends Spinner(FiniteDuration(100, "ms"),
  "▁▂▃▄▅▆▇█▇▆▅▄▃▂▁",
  "▂▃▄▅▆▇█▇█▇▆▅▄▃▂",
  "▃▄▅▆▇█▇▆▇█▇▆▅▄▃",
  "▄▅▆▇█▇▆▅▆▇█▇▆▅▄",
  "▅▆▇█▇▆▅▄▅▆▇█▇▆▅",
  "▆▇█▇▆▅▄▃▄▅▆▇█▇▆",
  "▇█▇▆▅▄▃▂▃▄▅▆▇█▇",
  "█▇▆▅▄▃▂▁▂▃▄▅▆▇█",
  "▇▆▅▄▃▂▁▂▁▂▃▄▅▆▇",
  "▆▅▄▃▂▁▂▃▂▁▂▃▄▅▆",
  "▅▄▃▂▁▂▃▄▃▂▁▂▃▄▅",
  "▄▃▂▁▂▃▄▅▄▃▂▁▂▃▄",
  "▃▂▁▂▃▄▅▆▅▄▃▂▁▂▃",
  "▂▁▂▃▄▅▆▇▆▅▄▃▂▁▂"
)
case class BrailSpinner() extends Spinner(FiniteDuration(100, "ms"), "⢎⡰", "⢎⡡", "⢎⡑", "⢎⠱", "⠎⡱", "⢊⡱", "⢌⡱", "⢆⡱")
case class ASCIISpinner() extends Spinner(FiniteDuration(200, "ms"), "|", "/", "—", "\\")
