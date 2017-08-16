package tlang.utils.formatting

import tlang.utils.Extensions._

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

  val NoColor = Color("", isActive = false)

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
