package tlang.formatting

import java.io.File

import tlang.Constants
import tlang.formatting.BoxStyles.{Ascii, BoxStyle}
import tlang.formatting.Colors.ColorScheme.DefaultColorScheme
import tlang.formatting.Colors.{Color, ColorScheme}
import tlang.options.Arguments.{ColorSchemeFlag, FormattingStyleFlag, LineWidthFlag, NoColorFlag}
import tlang.options.Options

object FancyFormatting extends Formatting(
  BoxStyles.Unicode, LineWidthFlag.DefaultWidth, useColor = true, asciiOnly = false
)
object SimpleFormatting extends Formatting(
  BoxStyles.Ascii, 80, useColor = false, asciiOnly = true
)

object Formatting {

  def apply(options: Options): Formatting = {
    val boxStyle = options(FormattingStyleFlag)
    Formatting(boxStyle, options(LineWidthFlag), options(ColorSchemeFlag), !options(NoColorFlag), boxStyle == Ascii)
  }
}

case class Formatting(
  boxStyle: BoxStyle = BoxStyles.Unicode,
  lineWidth: Int = LineWidthFlag.DefaultWidth,
  colorScheme: ColorScheme = DefaultColorScheme,
  useColor: Boolean = true,
  asciiOnly: Boolean = false) {

  /*--------------------------------- Colors --------------------------------*/

  val NoColor  : Color = Colors.NoColor
  val Reset    : Color = color(Colors.Reset, useColor)
  val Bold     : Color = color(Colors.Bold, useColor)
  val Underline: Color = color(Colors.Underlined, useColor)
  val Inverse  : Color = color(Colors.Inverse, useColor)

  val Black  : Color = color(Colors.Black, useColor)
  val Red    : Color = color(Colors.Red, useColor)
  val Green  : Color = color(Colors.Green, useColor)
  val Yellow : Color = color(Colors.Yellow, useColor)
  val Blue   : Color = color(Colors.Blue, useColor)
  val Magenta: Color = color(Colors.Magenta, useColor)
  val Cyan   : Color = color(Colors.Cyan, useColor)
  val White  : Color = color(Colors.White, useColor)

  val BlackBG  : Color = color(Colors.BlackBG, useColor)
  val RedBG    : Color = color(Colors.RedBG, useColor)
  val GreenBG  : Color = color(Colors.GreenBG, useColor)
  val YellowBG : Color = color(Colors.YellowBG, useColor)
  val BlueBG   : Color = color(Colors.BlueBG, useColor)
  val MagentaBG: Color = color(Colors.MagentaBG, useColor)
  val CyanBG   : Color = color(Colors.CyanBG, useColor)
  val WhiteBG  : Color = color(Colors.WhiteBG, useColor)

  val AllColors: List[Color] = List(Black, Red, Green, Yellow, Blue, Magenta, Cyan, White)

  /*------------------------------ Color Scheme -----------------------------*/

  val KeywordColor = color(Color(colorScheme.Keyword), useColor)
  val VarColor     = color(Color(colorScheme.Variable), useColor)
  val ClassColor   = color(Color(colorScheme.Class), useColor)
  val MethodColor  = color(Color(colorScheme.Method), useColor)
  val StringColor  = color(Color(colorScheme.String), useColor)
  val NumColor     = color(Color(colorScheme.Number), useColor)
  val CommentColor = color(Color(colorScheme.Comment), useColor)
  val SymbolColor  = color(Color(colorScheme.Symbol), useColor)

  val FileColor: Color = Bold + Magenta

  /*----------------------------- ASCII Variants ----------------------------*/

  def ListMarker: String = ascii("*", "•")
  def Cross: String = ascii("x", "×")
  def RightArrow: String = ascii("->", "→")
  def LeftArrow: String = ascii("<-", "←")
  def UnderlineCharacter: String = ascii("~", "‾")

  def spinner: Spinner = ascii(ASCIISpinner(), BrailSpinner())

  def formatFileName(file: Option[File]): String = {
    file match {
      case Some(f) => formatFileName(f)
      case None    => FileColor("No file")
    }
  }

  def formatFileName(file: File): String = formatFileName(file.getName.replaceAll("\\..*$", ""))

  def formatFileName(name: String): String = FileColor(name + Constants.FileEnding)

  def makeList(items: String*): String = makeList(items)
  def makeList(items: Traversable[String]): String = items.map(item => s"  $ListMarker $item").mkString("\n")


  private def ascii[T](ascii: T, nonAscii: T): T = if (asciiOnly) ascii else nonAscii
  private def color(color: Color, isActive: Boolean) = if (isActive) color else Colors.NoColor

}
