package tlang.formatting

import java.io.File

import tlang.Constants
import tlang.compiler.options.Flags.LineWidth
import tlang.formatting.BoxStyles.BoxStyle
import tlang.formatting.Colors.{Color, ColorScheme, DefaultColorScheme}

object FancyFormatting extends Formatting(
  BoxStyles.Unicode, LineWidth.DefaultWidth, useColor = true, asciiOnly = false
)
object SimpleFormatting extends Formatting(
  BoxStyles.Ascii, 80, useColor = false, asciiOnly = true
)

case class Formatting(
  boxStyle: BoxStyle = BoxStyles.Unicode,
  lineWidth: Int = LineWidth.DefaultWidth,
  colorScheme: ColorScheme = DefaultColorScheme,
  useColor: Boolean = true,
  asciiOnly: Boolean = false) {

  /*--------------------------------- Colors --------------------------------*/

  val NoColor  : Color = Colors.NoColor
  val Reset    : Color = Colors.Reset(useColor)
  val Bold     : Color = Colors.Bold(useColor)
  val Underline: Color = Colors.Underlined(useColor)
  val Inverse  : Color = Colors.Inverse(useColor)

  val Black  : Color = Colors.Black(useColor)
  val Red    : Color = Colors.Red(useColor)
  val Green  : Color = Colors.Green(useColor)
  val Yellow : Color = Colors.Yellow(useColor)
  val Blue   : Color = Colors.Blue(useColor)
  val Magenta: Color = Colors.Magenta(useColor)
  val Cyan   : Color = Colors.Cyan(useColor)
  val White  : Color = Colors.White(useColor)

  val BlackBG  : Color = Colors.BlackBG(useColor)
  val RedBG    : Color = Colors.RedBG(useColor)
  val GreenBG  : Color = Colors.GreenBG(useColor)
  val YellowBG : Color = Colors.YellowBG(useColor)
  val BlueBG   : Color = Colors.BlueBG(useColor)
  val MagentaBG: Color = Colors.MagentaBG(useColor)
  val CyanBG   : Color = Colors.CyanBG(useColor)
  val WhiteBG  : Color = Colors.WhiteBG(useColor)

  val AllColors: Array[Color] = Array(Black, Red, Green, White, Yellow, Blue, Magenta, Cyan, White)

  /*------------------------------ Color Scheme -----------------------------*/

  val KeywordColor     = Color(colorScheme.Keyword, useColor)
  val VarColor         = Color(colorScheme.Variable, useColor)
  val ClassColor       = Color(colorScheme.Class, useColor)
  val MethodColor      = Color(colorScheme.Method, useColor)
  val StringColor      = Color(colorScheme.String, useColor)
  val NumColor         = Color(colorScheme.Number, useColor)
  val CommentColor     = Color(colorScheme.Comment, useColor)
  val SymbolColor      = Color(colorScheme.Symbol, useColor)
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

}
