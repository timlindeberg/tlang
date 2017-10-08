package tlang.formatting

import tlang.formatting.Colors.ColorType.{Background, Foreground, Mixed, Modifier}

import scala.collection.Set
import scala.collection.mutable.ArrayBuffer

object Colors {

  val NO_COLOR   = -1
  val RESET      = 0
  val BOLD       = 1
  val UNDERLINED = 4
  val BLINK      = 5
  val INVERSE    = 7
  val CONCEALED  = 8

  val BLACK      = 30
  val RED        = 31
  val GREEN      = 32
  val YELLOW     = 33
  val BLUE       = 34
  val MAGENTA    = 35
  val CYAN       = 36
  val WHITE      = 37
  val DEFAULT_FG = 39

  val BLACK_BG   = 40
  val RED_BG     = 41
  val GREEN_BG   = 42
  val YELLOW_BG  = 43
  val BLUE_BG    = 44
  val MAGENTA_BG = 45
  val CYAN_BG    = 46
  val WHITE_BG   = 47
  val DEFAULT_BG = 49

  val Bold      : Color = RegularColor(modifiers = Set(BOLD))
  val Underlined: Color = RegularColor(modifiers = Set(UNDERLINED))
  val Inverse   : Color = RegularColor(modifiers = Set(INVERSE))

  val Black    : Color = RegularColor(foreground = BLACK)
  val Red      : Color = RegularColor(foreground = RED)
  val Green    : Color = RegularColor(foreground = GREEN)
  val Yellow   : Color = RegularColor(foreground = YELLOW)
  val Blue     : Color = RegularColor(foreground = BLUE)
  val Magenta  : Color = RegularColor(foreground = MAGENTA)
  val Cyan     : Color = RegularColor(foreground = CYAN)
  val White    : Color = RegularColor(foreground = WHITE)
  val DefaultFG: Color = RegularColor(foreground = DEFAULT_FG)

  val BlackBG  : Color = RegularColor(background = BLACK_BG)
  val RedBG    : Color = RegularColor(background = RED_BG)
  val GreenBG  : Color = RegularColor(background = GREEN_BG)
  val YellowBG : Color = RegularColor(background = YELLOW_BG)
  val BlueBG   : Color = RegularColor(background = BLUE_BG)
  val MagentaBG: Color = RegularColor(background = MAGENTA_BG)
  val CyanBG   : Color = RegularColor(background = CYAN_BG)
  val WhiteBG  : Color = RegularColor(background = WHITE_BG)
  val DefaultBG: Color = RegularColor(foreground = DEFAULT_BG)


  val ColorMap: Map[Int, Color] = Map(
    NO_COLOR -> NoColor,
    RESET -> Reset,
    BOLD -> Bold,
    UNDERLINED -> Underlined,
    INVERSE -> Inverse,

    BLACK -> Black,
    RED -> Red,
    GREEN -> Green,
    YELLOW -> Yellow,
    BLUE -> Blue,
    MAGENTA -> Magenta,
    CYAN -> Cyan,
    WHITE -> White,
    DEFAULT_FG -> DefaultFG,

    BLACK_BG -> BlackBG,
    RED_BG -> RedBG,
    GREEN_BG -> GreenBG,
    YELLOW_BG -> YellowBG,
    BLUE_BG -> BlueBG,
    MAGENTA_BG -> MagentaBG,
    CYAN_BG -> CyanBG,
    WHITE_BG -> WhiteBG,
    DEFAULT_BG -> DefaultBG
  )

  val ColorNameMap: Map[String, Int] = Map(
    "black" -> BLACK,
    "red" -> RED,
    "green" -> GREEN,
    "yellow" -> YELLOW,
    "blue" -> BLUE,
    "magenta" -> MAGENTA,
    "cyan" -> CYAN,
    "white" -> WHITE,
    "bold" -> BOLD,
    "underlined" -> UNDERLINED
  )


  val ColorNames: List[String] = ColorNameMap.keys.toList

  implicit class ColorableAny(val s: Any) extends AnyVal {
    def +(c: Color): String = s.toString + c.ansi
  }


  implicit def ColorToString(color: Color): String = color.ansi

  implicit def ColorToFunction(color: Color): String => String = color(_)


  trait ColorType
  object ColorType {
    case object NoColor extends ColorType
    case object Reset extends ColorType
    case object Foreground extends ColorType
    case object Background extends ColorType
    case object Modifier extends ColorType
    case object Mixed extends ColorType
  }

  trait Color {
    def ansi: String
    def repr: String = ansi.replace("\u001b", "\\u001b")

    def colorType: ColorType
    def +(c: Color): Color
    def +(any: Any): String
    def -(c: Color): Color
    def apply(any: Any): String
    override def toString: String = ansi
    def needsResetBefore(nextColor: Color): Boolean

    def foreground: Int
    def background: Int
    def modifiers: Set[Int]
  }

  case object NoColor extends Color {
    override val ansi     : String    = ""
    override val repr     : String    = "-"
    override val colorType: ColorType = ColorType.NoColor
    override def +(c: Color): Color = if (c == Reset) NoColor else c
    override def +(any: Any): String = any.toString
    override def -(c: Color): Color = NoColor
    override def apply(any: Any): String = any.toString
    override def needsResetBefore(nextColor: Color) = false

    override val foreground: Int      = -1
    override val background: Int      = -1
    override val modifiers : Set[Int] = Set()
  }

  case object Reset extends Color {
    override val ansi     : String    = "\u001b[0m"
    override val repr     : String    = "\\u001b[0m"
    override val colorType: ColorType = ColorType.Reset
    override def +(c: Color): Color = NoColor
    override def +(any: Any): String = ansi + any.toString
    override def -(c: Color): Color = Reset
    override def apply(any: Any): String = any.toString + ansi
    override def needsResetBefore(nextColor: Color) = false

    override val foreground: Int      = -1
    override val background: Int      = -1
    override val modifiers : Set[Int] = Set()

  }

  case object Color {

    def apply(code: Int): Color = ColorMap.getOrElse(code,
      throw new IllegalArgumentException(s"Illegal color code: $code")
    )

    def apply(ansiSequence: String): Color = {
      var ansi = ansiSequence
      if (ansi.startsWith("\u001b[") && ansi.endsWith("m"))
        ansi = ansi.substring(2, ansi.length - 1)

      val ansiValues = try {
        ansi.split(";").map(_.toInt)
      } catch {
        case _: NumberFormatException => throw new IllegalArgumentException(s"Not a valid ansi sequence: $ansiSequence")
      }

      ansiValues.foldLeft(NoColor: Color) { (color, value) => color + Color(value) }
    }
  }

  private case class RegularColor(override val foreground: Int = -1, override val background: Int = -1, override val modifiers: Set[Int] = Set()) extends Color {

    private val colors: List[Int] =
      (foreground :: background :: modifiers.toList)
        .filter(_ != -1)
        .sorted

    val ansi: String = colors.mkString("\u001b[", ";", "m")

    val colorType: ColorType = {
      (foreground, background, modifiers.size) match {
        case (x, -1, 0) if x != -1      => Foreground
        case (-1, x, 0) if x != -1      => Background
        case (-1, -1, mods) if mods > 0 => Modifier
        case _                          => Mixed
      }
    }

    def +(c: Color): Color = c match {
      case NoColor                                         => NoColor
      case Reset                                           => NoColor
      case RegularColor(foreground, background, modifiers) =>
        val fg = if (foreground != -1) foreground else this.foreground
        val bg = if (background != -1) background else this.background
        val mods = modifiers ++ this.modifiers
        RegularColor(fg, bg, mods)
    }

    def +(any: Any): String = ansi + any.toString

    def -(c: Color): Color = c match {
      case NoColor                                         => this
      case Reset                                           => this
      case RegularColor(foreground, background, modifiers) =>
        val fg = if (foreground == this.foreground) -1 else this.foreground
        val bg = if (background == this.background) -1 else this.background
        val mods = modifiers -- this.modifiers
        if (fg == -1 && bg == -1 && mods.isEmpty) NoColor else RegularColor(fg, bg, mods)
    }

    def apply(any: Any): String = {
      val s = any.toString
      if (s.isEmpty) s else ansi + any.toString + "\u001b[0m"
    }

    override def needsResetBefore(nextColor: Color): Boolean = nextColor match {
      case NoColor                                     => true
      case Reset                                       => false
      case RegularColor(nextFG, nextBG, nextModifiers) =>
        (colorType, nextColor.colorType) match {
          case (Foreground, Foreground) => false
          case (Background, Background) => false
          case (Foreground, Mixed)      => nextFG == -1
          case (Background, Mixed)      => nextBG == -1
          case (Modifier, Modifier)     => !modifiers.subsetOf(nextModifiers)
          case (Modifier, Mixed)        => !modifiers.subsetOf(nextModifiers)
          case (Mixed, Mixed)           =>
            (foreground != -1 && nextFG == -1) ||
              (background != -1 && nextBG == -1) ||
              (!modifiers.subsetOf(nextModifiers))
          case _                        => true
        }
    }

  }


  trait ColorScheme {

    import ColorScheme._

    def Keyword: Int
    def Variable: Int
    def Class: Int
    def Method: Int
    def String: Int
    def Number: Int
    def Comment: Int
    def Symbol: Int

    def colorMap = Map(
      KeywordName -> Keyword,
      VariableName -> Variable,
      ClassName -> Class,
      MethodName -> Method,
      StringName -> String,
      NumberName -> Number,
      CommentName -> Comment,
      SymbolName -> Symbol
    )

    def toJson: String = {
      val values = colorMap.map { case (key, value) => s"""""$key": "$value"""" }.mkString(",")
      s"{ $values }"
    }

  }

  object ColorScheme {

    val KeywordName  = "keyword"
    val VariableName = "variable"
    val ClassName    = "class"
    val MethodName   = "method"
    val StringName   = "string"
    val NumberName   = "number"
    val CommentName  = "comment"
    val SymbolName   = "symbol"

    val Keys: List[String] = List(
      KeywordName,
      VariableName,
      ClassName,
      MethodName,
      StringName,
      NumberName,
      CommentName,
      SymbolName
    )

    case object NoColors extends ColorScheme {
      val Keyword : Int = NO_COLOR
      val Variable: Int = NO_COLOR
      val Class   : Int = NO_COLOR
      val Method  : Int = NO_COLOR
      val String  : Int = NO_COLOR
      val Number  : Int = NO_COLOR
      val Comment : Int = NO_COLOR
      val Symbol  : Int = NO_COLOR
    }

    case object DefaultColorScheme extends ColorScheme {
      val Keyword : Int = BLUE
      val Variable: Int = CYAN
      val Class   : Int = GREEN
      val Method  : Int = YELLOW
      val String  : Int = YELLOW
      val Number  : Int = MAGENTA
      val Comment : Int = BLACK
      val Symbol  : Int = WHITE
    }
  }

  // Calculates the color of each character in the string
  // Returns the original string without any ansi escape sequences and
  // an array of the color each character.
  def splitStringAndColors(str: String): (String, Array[Color]) = {
    val colors = new ArrayBuffer[Color] {override val initialSize: Int = str.length }
    val sb = new StringBuilder(str.length)

    foreachWithColor(str) { (c, color) => sb += c; colors += color }
    (sb.toString, colors.toArray)
  }

  // Parses the ansi sequence starting at startIndex and returns the color
  // and the index where the ansi sequence ended
  def extractColorFrom(str: String, startIndex: Int, initialColor: Color = NoColor, extractMultiple: Boolean = true): (Color, Int) = {
    var color: Color = initialColor
    var i = startIndex
    while (i < str.length && str(i) == '\u001b' && str(i + 1) == '[') {
      val endOfAnsi = str.indexOf('m', i + 1)
      val ansiEscapeSequence = str.substring(i + 2, endOfAnsi)
      color += Color(ansiEscapeSequence)
      i = endOfAnsi + 1
      if (!extractMultiple)
        return (color, i)
    }
    (color, i)
  }

  def foreachWithColor[U](str: String)(f: (Char, Color) => U): Unit = {
    var color: Color = NoColor

    var i = 0
    while (i < str.length) {
      str(i) match {
        case '\u001b' if str(i + 1) == '[' =>
          val (newColor, endOfColor) = extractColorFrom(str, i)
          color += newColor
          i = endOfColor
        case _                             =>
      }
      if (i < str.length)
        f(str(i), color)

      i += 1
    }
  }

}

