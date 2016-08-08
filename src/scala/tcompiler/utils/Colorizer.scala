package tcompiler.utils

import scala.collection.mutable

/**
  * Created by timlindeberg on 23/07/16.
  */
trait Colorizer {

  def useColor: Boolean

  def Reset: String = GetColor(Console.RESET)

  def Bold: String = GetColor(Console.BOLD)
  def Underline: String = GetColor(Console.UNDERLINED)


  def Black: String = GetColor(Console.BLACK)
  def Red: String = GetColor(Console.RED)
  def Green: String = GetColor(Console.GREEN)
  def Yellow: String = GetColor(Console.YELLOW)
  def Blue: String = GetColor(Console.BLUE)
  def Magenta: String = GetColor(Console.MAGENTA)
  def Cyan: String = GetColor(Console.CYAN)
  def White: String = GetColor(Console.WHITE)

  def Colors = Array[String](Red, Green, White, Yellow, Blue, Reset, Magenta, Cyan)


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

  def KeywordColor: String = Blue
  def KeywordColor(s: Any): String = KeywordColor + s + Reset

  def VarColor: String = Cyan
  def VarColor(s: Any): String = VarColor + s + Reset

  def ClassColor: String = Green
  def ClassColor(s: Any): String = ClassColor + s + Reset

  def MethodColor: String = Yellow
  def MethodColor(s: Any): String = MethodColor + s + Reset

  def StringColor: String = Magenta
  def StringColor(s: Any): String = StringColor + s + Reset

  def NumColor: String = Magenta
  def NumColor(s: Any): String = NumColor + s + Reset

  private def GetColor(color: String) = if (useColor) color else ""

}
