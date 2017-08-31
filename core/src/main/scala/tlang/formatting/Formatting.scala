package tlang.formatting

import java.io.File

import tlang.Constants
import tlang.formatting.Colors.ColorScheme.DefaultColorScheme
import tlang.formatting.Colors.{Color, ColorScheme}
import tlang.options.Options
import tlang.options.arguments.{ColorSchemeFlag, FormattingStyleFlag, LineWidthFlag, NoColorFlag}

object FancyFormatting extends Formatting(FormattingStyles.Unicode, LineWidthFlag.DefaultWidth, useColor = true)
object SimpleFormatting extends Formatting(FormattingStyles.Ascii, 80, useColor = false)

object Formatting {

  def apply(options: Options): Formatting = {
    val formattingStyle = options(FormattingStyleFlag)
    Formatting(formattingStyle, options(LineWidthFlag), options(ColorSchemeFlag), !options(NoColorFlag))
  }

}

case class Formatting(
  formattingStyle: FormattingStyle = FormattingStyles.Unicode,
  lineWidth: Int = LineWidthFlag.DefaultWidth,
  colorScheme: ColorScheme = DefaultColorScheme,
  useColor: Boolean = true) {

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

  val KeywordColor: Color = color(Color(colorScheme.Keyword), useColor)
  val VarColor    : Color = color(Color(colorScheme.Variable), useColor)
  val ClassColor  : Color = color(Color(colorScheme.Class), useColor)
  val MethodColor : Color = color(Color(colorScheme.Method), useColor)
  val StringColor : Color = color(Color(colorScheme.String), useColor)
  val NumColor    : Color = color(Color(colorScheme.Number), useColor)
  val CommentColor: Color = color(Color(colorScheme.Comment), useColor)
  val SymbolColor : Color = color(Color(colorScheme.Symbol), useColor)

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
  def makeList(items: Traversable[String]): String =
    items
      .map(item => s"  ${ Bold(ListMarker) } $item")
      .mkString(System.lineSeparator)


  private def ascii[T](ascii: => T, nonAscii: => T): T = if (formattingStyle.asciiOnly) ascii else nonAscii
  private def color(color: Color, isActive: Boolean) = if (isActive) color else Colors.NoColor

}
