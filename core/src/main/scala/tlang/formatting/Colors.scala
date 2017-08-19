package tlang.formatting

object Colors {

  val RESET      = 0
  val BOLD       = 1
  val UNDERLINED = 4
  val INVERSE    = 7

  val BLACK   = 30
  val RED     = 31
  val GREEN   = 32
  val YELLOW  = 33
  val BLUE    = 34
  val MAGENTA = 35
  val CYAN    = 36
  val WHITE   = 37

  val BLACK_B   = 40
  val RED_B     = 41
  val GREEN_B   = 42
  val YELLOW_B  = 43
  val BLUE_B    = 44
  val MAGENTA_B = 45
  val CYAN_B    = 46
  val WHITE_B   = 47


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

  val NoColor = Color(-1, isActive = false)


  def Reset(implicit isActive: Boolean = true) = Color(RESET, isActive)
  def Bold(implicit isActive: Boolean = true) = Color(BOLD, isActive)
  def Underlined(implicit isActive: Boolean = true) = Color(UNDERLINED, isActive)
  def Inverse(implicit isActive: Boolean = true) = Color(INVERSE, isActive)

  def Black(implicit isActive: Boolean = true) = Color(BLACK, isActive)
  def Red(implicit isActive: Boolean = true) = Color(RED, isActive)
  def Green(implicit isActive: Boolean = true) = Color(GREEN, isActive)
  def Yellow(implicit isActive: Boolean = true) = Color(YELLOW, isActive)
  def Blue(implicit isActive: Boolean = true) = Color(BLUE, isActive)
  def Magenta(implicit isActive: Boolean = true) = Color(MAGENTA, isActive)
  def Cyan(implicit isActive: Boolean = true) = Color(CYAN, isActive)
  def White(implicit isActive: Boolean = true) = Color(WHITE, isActive)

  def BlackBG(implicit isActive: Boolean = true) = Color(BLACK_B, isActive)
  def RedBG(implicit isActive: Boolean = true) = Color(RED_B, isActive)
  def GreenBG(implicit isActive: Boolean = true) = Color(GREEN_B, isActive)
  def YellowBG(implicit isActive: Boolean = true) = Color(YELLOW_B, isActive)
  def BlueBG(implicit isActive: Boolean = true) = Color(BLUE_B, isActive)
  def MagentaBG(implicit isActive: Boolean = true) = Color(MAGENTA_B, isActive)
  def CyanBG(implicit isActive: Boolean = true) = Color(CYAN_B, isActive)
  def WhiteBG(implicit isActive: Boolean = true) = Color(WHITE_B, isActive)


  val ColorNames: List[String] = ColorNameMap.keys.toList

  def getColor(color: String): Option[Int] = {
    if (color.isEmpty)
      return Some(-1)

    if (color forall Character.isDigit)
      return Some(color.toInt)

    ColorNameMap.get(color)
  }

  implicit class ColorString(val s: Any) extends AnyVal {
    def +(c: Color): String = s.toString + c.ansi
  }

  implicit def ColorToString(c: Color): String = c.ansi

  case object Color {
    def apply(code: Int, isActive: Boolean): Color = Color(Set(code), isActive)
  }

  case class Color(colors: Set[Int], isActive: Boolean) {

    def ansi: String = {
      if (!isActive || colors.isEmpty)
        return ""

      colors.toList.filter(_ != -1).sorted.mkString("\u001b[", ";", "m")
    }

    override def equals(obj: scala.Any): Boolean = (this, obj) match {
      case (Color(_, false), Color(_, false)) => true
      case (Color(c1, true), Color(c2, true)) => c1 == c2
      case _                                  => false
    }

    override def toString: String = ansi

    def +(c: Color): Color = Color(colors ++ c.colors, isActive || c.isActive)
    def +(s: Any): String = ansi + s.toString
    def apply(any: Any): String = {
      val s = any.toString
      if (s.isEmpty || !isActive)
        return s
      ansi + s + "\u001b[0m"
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

  case object NoColors extends ColorScheme {
    val Keyword : Int = -1
    val Variable: Int = -1
    val Class   : Int = -1
    val Method  : Int = -1
    val String  : Int = -1
    val Number  : Int = -1
    val Comment : Int = -1
    val Symbol  : Int = -1
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
