package tlang.utils

import tlang.utils.Colors.{Color, ColorScheme, DefaultColorScheme}
import tlang.utils.Extensions._

import scala.Console._

object Colors {

  import Console._

  val ColorNameMap = Map(
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

  def getColor(color: String): Option[String] = {
    if (color.isEmpty)
      return Some("")

    if (color.isAnsi)
      return Some(color)

    ColorNameMap.get(color)
  }

  implicit class ColorString(val s: Any) extends AnyVal {
    def +(c: Color): String = s.toString + c.value
  }

  implicit def ColorToString(c: Color): String = c.value

  case object Color {
    def apply(s: String, isActive: Boolean): Color = Color(Set(s), isActive)
  }

  case class Color(colors: Set[String], isActive: Boolean) {

    def value: String = if (isActive) colors.mkString else ""
    override def toString: String = value

    def +(c: Color): Color = Color(colors ++ c.colors, isActive || c.isActive)
    def +(s: Any): String = value + s.toString
    def apply(any: Any): String = {
      val s = any.toString
      if (s.isEmpty || !isActive)
        return s
      value + s + RESET
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

    val ColorSchemeNames: List[String] = List(
      KeywordName,
      VariableName,
      ClassName,
      MethodName,
      StringName,
      NumberName,
      CommentName,
      SymbolName
    )
  }

  trait ColorScheme {

    import ColorScheme._

    def Keyword: String
    def Variable: String
    def Class: String
    def Method: String
    def String: String
    def Number: String
    def Comment: String
    def Symbol: String

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

  case object NoColors extends ColorScheme {
    val Keyword : String = ""
    val Variable: String = ""
    val Class   : String = ""
    val Method  : String = ""
    val String  : String = ""
    val Number  : String = ""
    val Comment : String = ""
    val Symbol  : String = ""
  }

  case object DefaultColorScheme extends ColorScheme {
    val Keyword : String = BLUE
    val Variable: String = CYAN
    val Class   : String = GREEN
    val Method  : String = YELLOW
    val String  : String = YELLOW
    val Number  : String = MAGENTA
    val Comment : String = BLACK
    val Symbol  : String = WHITE
  }

}

case class Colors(isActive: Boolean, colorScheme: ColorScheme = DefaultColorScheme) {

  val Reset     = Color(RESET, isActive)
  val Bold      = Color(BOLD, isActive)
  val Underline = Color(UNDERLINED, isActive)

  val Black   = Color(BLACK, isActive)
  val Red     = Color(RED, isActive)
  val Green   = Color(GREEN, isActive)
  val Yellow  = Color(YELLOW, isActive)
  val Blue    = Color(BLUE, isActive)
  val Magenta = Color(MAGENTA, isActive)
  val Cyan    = Color(CYAN, isActive)
  val White   = Color(WHITE, isActive)

  val BlackBG   = Color(BLACK_B, isActive)
  val RedBG     = Color(RED_B, isActive)
  val GreenBG   = Color(GREEN_B, isActive)
  val YellowBG  = Color(YELLOW_B, isActive)
  val BlueBG    = Color(BLUE_B, isActive)
  val MagentaBG = Color(MAGENTA_B, isActive)
  val CyanBG    = Color(CYAN_B, isActive)
  val WhiteBG   = Color(WHITE_B, isActive)

  val AllColors: Array[Color] = Array(Red, Green, White, Yellow, Blue, Reset, Magenta, Cyan)

  // Color scheme

  val KeywordColor = Color(colorScheme.Keyword, isActive)
  val VarColor     = Color(colorScheme.Variable, isActive)
  val ClassColor   = Color(colorScheme.Class, isActive)
  val MethodColor  = Color(colorScheme.Method, isActive)
  val StringColor  = Color(colorScheme.String, isActive)
  val NumColor     = Color(colorScheme.Number, isActive)
  val CommentColor = Color(colorScheme.Comment, isActive)
  val SymbolColor  = Color(colorScheme.Symbol, isActive)
  val NoColor      = Color("", isActive = false)
}
