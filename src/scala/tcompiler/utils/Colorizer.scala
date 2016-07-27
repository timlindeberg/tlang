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


  private def GetColor(color: String) = if (useColor && System.console() != null) color else ""

}
