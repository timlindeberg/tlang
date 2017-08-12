package tlang.utils.formatting

import java.io.File

import tlang.Constants
import tlang.compiler.ast.{PrettyPrinter, TreePrinter}
import tlang.compiler.options.Flags.LineWidth
import tlang.utils.formatting.BoxStyles.BoxStyle
import tlang.utils.formatting.Colors.{Color, ColorScheme, DefaultColorScheme}

object FancyFormatting extends Formatting(
  BoxStyles.Unicode, LineWidth.DefaultWidth, useColor = true, asciiOnly = false
)
object SimpleFormatting extends Formatting(
  BoxStyles.Ascii, LineWidth.DefaultWidth, useColor = false, asciiOnly = true
)

case class Formatting(
  boxStyle: BoxStyle = BoxStyles.Unicode,
  lineWidth: Int = LineWidth.DefaultWidth,
  colorScheme: ColorScheme = DefaultColorScheme,
  useColor: Boolean = true,
  asciiOnly: Boolean = false) {

  import Console._

  /*--------------------------------- Colors --------------------------------*/

  val NoColor   = Color("", isActive = false)
  val Reset     = Color(RESET, useColor)
  val Bold      = Color(BOLD, useColor)
  val Underline = Color(UNDERLINED, useColor)

  val Black   = Color(BLACK, useColor)
  val Red     = Color(RED, useColor)
  val Green   = Color(GREEN, useColor)
  val Yellow  = Color(YELLOW, useColor)
  val Blue    = Color(BLUE, useColor)
  val Magenta = Color(MAGENTA, useColor)
  val Cyan    = Color(CYAN, useColor)
  val White   = Color(WHITE, useColor)

  val BlackBG   = Color(BLACK_B, useColor)
  val RedBG     = Color(RED_B, useColor)
  val GreenBG   = Color(GREEN_B, useColor)
  val YellowBG  = Color(YELLOW_B, useColor)
  val BlueBG    = Color(BLUE_B, useColor)
  val MagentaBG = Color(MAGENTA_B, useColor)
  val CyanBG    = Color(CYAN_B, useColor)
  val WhiteBG   = Color(WHITE_B, useColor)

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

  /*------------------------------- Utilities -------------------------------*/

  val wordWrapper           = AnsiWordWrapper()
  val syntaxHighlighter     = SyntaxHighlighter(this)
  val stackTraceHighlighter = StackTraceHighlighter(this)
  val prettyPrinter         = PrettyPrinter(this)
  val treePrinter           = TreePrinter(this)

  /*----------------------------- ASCII Variants ----------------------------*/

  def ListMarker: String = ascii("*", "•")
  def Cross: String = ascii("x", "×")
  def RightArrow: String = ascii("-", "→")
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
