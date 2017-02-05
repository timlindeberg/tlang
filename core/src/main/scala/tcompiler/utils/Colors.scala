package tcompiler.utils

import tcompiler.utils.Extensions._

/**
  * Created by Tim Lindeberg on 1/27/2017.
  */
object Colors {

  val ColorNameMap = Map(
    "black" -> Console.BLACK,
    "red" -> Console.RED,
    "green" -> Console.GREEN,
    "yellow" -> Console.YELLOW,
    "blue" -> Console.BLUE,
    "magenta" -> Console.MAGENTA,
    "cyan" -> Console.CYAN,
    "white" -> Console.WHITE,
    "bold" -> Console.BOLD,
    "underlined" -> Console.UNDERLINED
  )

  val ColorNames: List[String] = ColorNameMap.keys.toList

  def getColor(color: String): Option[String] = {
    if (color.isEmpty)
      return Some("")

    if (color.isAnsi)
      return Some(color)

    ColorNameMap.get(color)
  }

}

case class Colors(active: Boolean, colorScheme: ColorScheme = DefaultColorScheme) {

  val Reset: String = _getColor(Console.RESET)

  val Bold     : String = _getColor(Console.BOLD)
  val Underline: String = _getColor(Console.UNDERLINED)

  val Black  : String = _getColor(Console.BLACK)
  val Red    : String = _getColor(Console.RED)
  val Green  : String = _getColor(Console.GREEN)
  val Yellow : String = _getColor(Console.YELLOW)
  val Blue   : String = _getColor(Console.BLUE)
  val Magenta: String = _getColor(Console.MAGENTA)
  val Cyan   : String = _getColor(Console.CYAN)
  val White  : String = _getColor(Console.WHITE)

  val AllColors: Array[String] = Array(Red, Green, White, Yellow, Blue, Reset, Magenta, Cyan)

  def Underline(s: Any): String = Underline + s + Reset
  def Bold(s: Any): String = Bold + s + Reset
  def Black(s: Any): String = Black + s + Reset
  def Red(s: Any): String = Red + s + Reset
  def Green(s: Any): String = Green + s + Reset
  def Yellow(s: Any): String = Yellow + s + Reset
  def Blue(s: Any): String = Blue + s + Reset
  def Magenta(s: Any): String = Magenta + s + Reset
  def Cyan(s: Any): String = Cyan + s + Reset
  def White(s: Any): String = White + s + Reset

  // Color scheme

  val KeywordColor: String = _getColor(colorScheme.Keyword)
  def KeywordColor(s: Any): String = KeywordColor + s + Reset

  val VarColor: String = _getColor(colorScheme.Variable)
  def VarColor(s: Any): String = VarColor + s + Reset

  val ClassColor: String = _getColor(colorScheme.Class)
  def ClassColor(s: Any): String = ClassColor + s + Reset

  val MethodColor: String = _getColor(colorScheme.Method)
  def MethodColor(s: Any): String = MethodColor + s + Reset

  val StringColor: String = _getColor(colorScheme.String)
  def StringColor(s: Any): String = StringColor + s + Reset

  val NumColor: String = _getColor(colorScheme.Number)
  def NumColor(s: Any): String = NumColor + s + Reset

  val CommentColor: String = _getColor(colorScheme.Comment)
  def CommentColor(s: Any): String = CommentColor + s + Reset

  val SymbolColor: String = _getColor(colorScheme.Symbol)
  def SymbolColor(s: Any): String = SymbolColor + s + Reset

  private def _getColor(color: String) = if (active) color else ""

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
  val Keyword : String = Console.BLUE
  val Variable: String = Console.CYAN
  val Class   : String = Console.GREEN
  val Method  : String = Console.YELLOW
  val String  : String = Console.YELLOW
  val Number  : String = Console.MAGENTA
  val Comment : String = Console.BLACK
  val Symbol  : String = Console.WHITE
}