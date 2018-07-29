package tlang
package formatting


import java.nio.file.Path

import better.files.File
import tlang.Constants
import tlang.formatting.Colors.ColorScheme.DefaultColorScheme
import tlang.formatting.Colors.{Color, ColorScheme}
import tlang.formatting.grid.Grid
import tlang.formatting.textformatters._
import tlang.options.argument.{LineWidthFlag, TabWidthFlag}


object Formatter {

  val SimpleFormatter: Formatter = Formatter(
    80,
    TabWidthFlag.defaultValue,
    useColor = false,
    asciiOnly = true
  )
  val PrettyFormatter: Formatter = Formatter(
    LineWidthFlag.DefaultWidth,
    TabWidthFlag.defaultValue,
    useColor = true,
    asciiOnly = false
  )

  def apply(
    lineWidth: Int = LineWidthFlag.DefaultWidth,
    tabWidth: Int = TabWidthFlag.defaultValue,
    colorScheme: ColorScheme = DefaultColorScheme,
    useColor: Boolean = true,
    asciiOnly: Boolean = false
  ): Formatter = {
    Formatter(
      WordWrapper(wrapAnsiColors = useColor),
      Truncator(),
      TabReplacer(tabWidth),
      lineWidth,
      colorScheme,
      useColor,
      asciiOnly
    )
  }
}

case class Formatter(
  wordWrap: WordWrapper,
  truncate: Truncator,
  replaceTabs: TabReplacer,
  var lineWidth: Int, // this is a var so we can change line width dynamically in trepl
  colorScheme: ColorScheme,
  useColor: Boolean,
  asciiOnly: Boolean
) {

  def grid: Grid = Grid()(this)

  def splitWithColors(str: String): List[String] = {
    val lines = str.split("\r?\n", -1).toList
    wordWrap.wrapAnsiFormatting(lines)
  }

  def fileName(file: File): String = fileName(file.nameWithoutExtension)

  def fileName(name: String): String = {
    val color = Bold + Magenta
    color(name + Constants.FileEnding)
  }

  def relativePath(file: File): String = {
    val fileName = file.name
    relativize(file.parent.path) + file.fileSystem.getSeparator + fileName
  }

  def list(items: String*): String = list(items)
  def list(items: Traversable[String]): String =
    items
      .map(item => s"  ${ Bold(ListMarker) } $item")
      .mkString(NL)

  def translate(c: Color): Color = color(c)

  /*--------------------------------- Colors --------------------------------*/

  val NoColor  : Color = Colors.NoColor
  val Reset    : Color = color(Colors.Reset)
  val Bold     : Color = color(Colors.Bold)
  val Underline: Color = color(Colors.Underline)
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
  val BGColors: List[Color] = List(
    BlackBG + White,
    RedBG + Black,
    GreenBG + Black,
    YellowBG + Black,
    BlueBG + Black,
    MagentaBG + Black,
    CyanBG + Black,
    WhiteBG + Black
  )

  val AllColors: List[Color] = FGColors ++ BGColors

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

  private def relativize(path: Path): Path = {
    val absolute = path.toAbsolutePath
    if (absolute.startsWith(Constants.Pwd)) Constants.Pwd.relativize(absolute) else absolute
  }

}
