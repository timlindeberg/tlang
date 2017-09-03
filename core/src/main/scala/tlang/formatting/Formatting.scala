package tlang.formatting

import tlang.formatting.Colors.ColorScheme.DefaultColorScheme
import tlang.formatting.Colors.{Color, ColorScheme}
import tlang.options.Options
import tlang.options.arguments.{AsciiFlag, ColorSchemeFlag, LineWidthFlag, NoColorFlag}

object DefaultFormatting extends Formatting(LineWidthFlag.DefaultWidth, useColor = true, asciiOnly = false)
object SimpleFormatting extends Formatting(80, useColor = false, asciiOnly = true)

object Formatting {

  def apply(options: Options): Formatting = {
    Formatting(options(LineWidthFlag), options(ColorSchemeFlag), !options(NoColorFlag), options(AsciiFlag))
  }

}

case class Formatting(
  lineWidth: Int = LineWidthFlag.DefaultWidth,
  colorScheme: ColorScheme = DefaultColorScheme,
  useColor: Boolean = true,
  asciiOnly: Boolean = false) {

  /*--------------------------------- Colors --------------------------------*/

  val NoColor  : Color = Colors.NoColor
  val Reset    : Color = color(Colors.Reset)
  val Bold     : Color = color(Colors.Bold)
  val Underline: Color = color(Colors.Underlined)
  val Inverse  : Color = color(Colors.Inverse)

  val Black  : Color = color(Colors.Black)
  val Red    : Color = color(Colors.Red)
  val Green  : Color = color(Colors.Green)
  val Yellow : Color = color(Colors.Yellow)
  val Blue   : Color = color(Colors.Blue)
  val Magenta: Color = color(Colors.Magenta)
  val Cyan   : Color = color(Colors.Cyan)
  val White  : Color = color(Colors.White)

  val BlackBG  : Color = color(Colors.BlackBG)
  val RedBG    : Color = color(Colors.RedBG)
  val GreenBG  : Color = color(Colors.GreenBG)
  val YellowBG : Color = color(Colors.YellowBG)
  val BlueBG   : Color = color(Colors.BlueBG)
  val MagentaBG: Color = color(Colors.MagentaBG)
  val CyanBG   : Color = color(Colors.CyanBG)
  val WhiteBG  : Color = color(Colors.WhiteBG)

  val FGColors: List[Color] = List(Black, Red, Green, Yellow, Blue, Magenta, Cyan, White)

  /*------------------------------ Color Scheme -----------------------------*/

  val KeywordColor: Color = color(Color(colorScheme.Keyword))
  val VarColor    : Color = color(Color(colorScheme.Variable))
  val ClassColor  : Color = color(Color(colorScheme.Class))
  val MethodColor : Color = color(Color(colorScheme.Method))
  val StringColor : Color = color(Color(colorScheme.String))
  val NumColor    : Color = color(Color(colorScheme.Number))
  val CommentColor: Color = color(Color(colorScheme.Comment))
  val SymbolColor : Color = color(Color(colorScheme.Symbol))


  /*----------------------------- ASCII Variants ----------------------------*/

  def spinner: Spinner = ascii(ASCIISpinner(), BrailSpinner())

  val Horizontal             : String = ascii("-", "─")
  val Vertical               : String = ascii("|", "│")
  val TopLeft                : String = ascii(" ", "┌")
  val TopRight               : String = ascii(" ", "┐")
  val BottomRight            : String = ascii(" ", "┘")
  val BottomLeft             : String = ascii(" ", "└")
  val HorizontalDown         : String = ascii("-", "┬")
  val HorizontalUp           : String = ascii("-", "┴")
  val VerticalRight          : String = ascii("|", "├")
  val VerticalLeft           : String = ascii("|", "┤")
  val HorizontalVertical     : String = ascii("+", "┼")
  val HorizontalThick        : String = ascii("=", "═")
  val TopLeftThick           : String = ascii(" ", "╒")
  val TopRightThick          : String = ascii(" ", "╕")
  val BottomLeftThick        : String = ascii(" ", "╘")
  val BottomRightThick       : String = ascii(" ", "╛")
  val VerticalRightThick     : String = ascii("|", "╞")
  val VerticalLeftThick      : String = ascii("|", "╡")
  val HorizontalDownThick    : String = ascii("=", "╤")
  val HorizontalUpThick      : String = ascii("=", "╧")
  val HorizontalVerticalThick: String = ascii("+", "╪")

  val ListMarker        : String = ascii("*", "•")
  val Cross             : String = ascii("x", "×")
  val RightArrow        : String = ascii("->", "→")
  val LeftArrow         : String = ascii("<-", "←")
  val UnderlineCharacter: String = ascii("~", "‾")


  private def ascii[T](ascii: => T, nonAscii: => T): T = if (asciiOnly) ascii else nonAscii
  private def color(color: Color): Color = if (useColor) color else Colors.NoColor

}
