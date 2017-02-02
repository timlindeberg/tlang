package tcompiler.utils

/**
  * Created by Tim Lindeberg on 1/27/2017.
  */
case class Colorizer(useColor: Boolean) {

  val Reset: String = GetColor(Console.RESET)

  val Bold     : String = GetColor(Console.BOLD)
  val Underline: String = GetColor(Console.UNDERLINED)

  val Black  : String = GetColor(Console.BLACK)
  val Red    : String = GetColor(Console.RED)
  val Green  : String = GetColor(Console.GREEN)
  val Yellow : String = GetColor(Console.YELLOW)
  val Blue   : String = GetColor(Console.BLUE)
  val Magenta: String = GetColor(Console.MAGENTA)
  val Cyan   : String = GetColor(Console.CYAN)
  val White  : String = GetColor(Console.WHITE)

  val Colors: Array[String] = Array(Red, Green, White, Yellow, Blue, Reset, Magenta, Cyan)

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

  val KeywordColor: String = Blue
  def KeywordColor(s: Any): String = KeywordColor + s + Reset

  val VarColor: String = Cyan
  def VarColor(s: Any): String = VarColor + s + Reset

  val ClassColor: String = Green
  def ClassColor(s: Any): String = ClassColor + s + Reset

  val MethodColor: String = Yellow
  def MethodColor(s: Any): String = MethodColor + s + Reset

  val StringColor: String = Yellow
  def StringColor(s: Any): String = StringColor + s + Reset

  val NumColor: String = Magenta
  def NumColor(s: Any): String = NumColor + s + Reset

  val CommentColor: String = White
  def CommentColor(s: Any): String = CommentColor + s + Reset

  private def GetColor(color: String) = if (useColor) color else ""

}
