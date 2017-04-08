package tlang.utils.formatting

import tlang.utils.Extensions._
import tlang.utils.formatting.Colors.Color

import scala.collection.generic.CanBuildFrom
import scala.collection.mutable.ArrayBuffer
import scala.collection.{IndexedSeqLike, mutable}

case class ColoredCharacter(color: Color, char: Char)

object ColoredString {

  def apply(formatting: Formatting, s: String) = new ColoredString(formatting, s)

  def newBuilder(formatting: Formatting): mutable.Builder[ColoredCharacter, ColoredString] = new mutable.Builder[ColoredCharacter, ColoredString] {
    val buff = new ArrayBuffer[ColoredCharacter]()
    override def +=(elem: ColoredCharacter): this.type = {
      buff += elem
      this
    }
    override def clear(): Unit = buff.clear()
    override def result(): ColoredString = new ColoredString(formatting, buff)
  }

  implicit def canBuildFrom: CanBuildFrom[ColoredString, ColoredCharacter, ColoredString] =
    new CanBuildFrom[ColoredString, ColoredCharacter, ColoredString] {
      override def apply(from: ColoredString): mutable.Builder[ColoredCharacter, ColoredString] = newBuilder(from.formatting)
      override def apply(): mutable.Builder[ColoredCharacter, ColoredString] = newBuilder(SimpleFormatting)
    }

  def getColoredChars(formatting: Formatting, s: String): ArrayBuffer[ColoredCharacter] = {

    import formatting._

    var currentColor: Color = NoColor
    var currentBGColor: Color = NoColor
    var SGRs = mutable.Set[Color]()
    val coloredChars = mutable.ArrayBuffer[ColoredCharacter]()

    var i = 0
    while (i < s.length) {
      s(i) match {
        case '\u001b' if s(i + 1) == '[' =>
          val endOfAnsi = s.indexOf('m', i + 1)
          val ansi = s.subSequence(i + 2, endOfAnsi).toString
          ansi.split(":").map(_.toList).foreach {
            case '0' :: Nil                           =>
              currentColor = NoColor
              currentBGColor = NoColor
              SGRs.clear()
            case '1' :: Nil                           => SGRs += Bold
            case '4' :: Nil                           => SGRs += Underline
            case '3' :: c :: Nil if c in ('1' to '7') => currentColor = getAnsiColor(formatting, c, isBG = false)
            case '4' :: c :: Nil if c in ('1' to '7') => currentBGColor = getAnsiColor(formatting, c, isBG = true)
            case _                                    =>
          }

          i = endOfAnsi
        case c                           =>
          val a = currentBGColor + currentColor
          val color = SGRs.foldLeft(a) { (current, sgr) => current + sgr }
          coloredChars += ColoredCharacter(color, c)
      }
      i += 1
    }
    coloredChars
  }

  private def getAnsiColor(formatting: Formatting, char: Char, isBG: Boolean) = {
    import formatting._
    char match {
      case '0' => if (isBG) BlackBG else Black
      case '1' => if (isBG) RedBG else Red
      case '2' => if (isBG) GreenBG else Green
      case '3' => if (isBG) YellowBG else Yellow
      case '4' => if (isBG) BlueBG else Blue
      case '5' => if (isBG) MagentaBG else Magenta
      case '6' => if (isBG) CyanBG else Cyan
      case '7' => if (isBG) WhiteBG else White
      case _   => ???
    }
  }


}
case class ColoredString(formatting: Formatting, chars: IndexedSeq[ColoredCharacter])
  extends IndexedSeq[ColoredCharacter] with IndexedSeqLike[ColoredCharacter, ColoredString] {

  def this(formatting: Formatting, s: String) = this(formatting, ColoredString.getColoredChars(formatting, s))

  import formatting._

  override def apply(idx: Int): ColoredCharacter = chars(idx)

  def toAnsiString: String = {
    val sb = new StringBuilder(chars.length)
    var i = 0
    var previousColor = chars(0).color
    sb ++= previousColor
    while (i < chars.length) {
      val cc = chars(i)
      val color = cc.color
      if (color != previousColor) {
        sb ++= Reset
        sb ++= color
      }
      sb += cc.char
      i += 1
      previousColor = color
    }
    if (chars.last.color != NoColor)
      sb ++= Reset
    sb.toString()
  }

  override def newBuilder: mutable.Builder[ColoredCharacter, ColoredString] = ColoredString.newBuilder(formatting)

  override def foreach[U](f: (ColoredCharacter) => U): Unit = chars.foreach(f)
  override def length: Int = chars.length
}
