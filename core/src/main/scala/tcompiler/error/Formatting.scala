package tcompiler.error

import tcompiler.error.Formats.{Box, Simple}
import tcompiler.utils.Extensions._
import tcompiler.utils.{Colorizer, Enumeration}

/**
  * Created by Tim Lindeberg on 1/29/2017.
  */

case class Formatting(box: Box, colorizer: Colorizer)

object SimpleFormatting extends Formatting(Simple, new Colorizer(useColor = false))


object Formats {
  sealed abstract class Box(val chars: String) extends Product with Serializable {
    val ─ : String = chars(0).toString
    val │ : String = chars(1).toString
    val ┌ : String = chars(2).toString
    val ┐ : String = chars(3).toString
    val ┘ : String = chars(4).toString
    val └ : String = chars(5).toString
    val ┬ : String = chars(6).toString
    val ┴ : String = chars(7).toString
    val ├ : String = chars(8).toString
    val ┤ : String = chars(9).toString
    val ┼ : String = chars(10).toString

    val name: String = getClass.getSimpleName.dropRight(1).toLowerCase

    def top(width: Int): String = ┌ + ─ * (width - 2) + ┐ + "\n"
    def bottom(width: Int): String = └ + ─ * (width - 2) + ┘ + "\n"
    def seperator(left: String, bridge: String, right: String, bridgeAt: Int, width: Int): String = {
      val rest = ─ * (width - bridgeAt - 5)
      val overNumbers = ─ * (bridgeAt + 2)
      left + overNumbers + bridge + rest + right + "\n"
    }

    def makeLine(line: String, width: Int): String = {
      val whitespaces = " " * (width - line.charCount)
      │ + " " + line + whitespaces + " " + │ + "\n"
    }

    def makeLines(lines: List[String], width: Int): String = lines.map(makeLine(_, width)).mkString

  }


  // @formatter:off
  case object Simple             extends Box("-|||||--||-")
  case object NoLines            extends Box("           ")
  case object Double             extends Box("═║╔╗╝╚╦╩╠╣╬")
  case object Light              extends Box("─│┌┐┘└┬┴├┤┼")
  case object Heavy              extends Box("━┃┏┓┛┗┳┻┣┫╋")
  case object DoubleDashLight    extends Box("╌╎┌┐┘└┬┴├┤┼")
  case object DoubleDashHeavy    extends Box("╍╏┏┓┛┗┳┻┣┫╋")
  case object TripleDashLight    extends Box("┄┆┌┐┘└┬┴├┤┼")
  case object TripleDashHeavy    extends Box("┅┇┏┓┛┗┳┻┣┫╋")
  case object QuadrupleDashLight extends Box("┈┊┌┐┘└┬┴├┤┼")
  case object QuadrupleDashHeavy extends Box("┉┋┏┓┛┗┳┻┣┫╋")
  // @formatter:on

  lazy val Types: Set[Box] = Enumeration.instancesOf[Box]
}
