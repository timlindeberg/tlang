package tcompiler.error

import tcompiler.error.Boxes.{Box, Simple}
import tcompiler.utils.Extensions._
import tcompiler.utils.{Colorizer, Enumeration}

/**
  * Created by Tim Lindeberg on 1/29/2017.
  */

case class Formatting(box: Box, width: Int, colorizer: Colorizer) {

  import box._

  private val wordWrapper = new AnsiWordWrapper

  def top: String = ┌ + ─ * (width - 2) + ┐ + "\n"
  def bottom: String = └ + ─ * (width - 2) + ┘ + "\n"
  def divider: String = ├ + ─ * (width - 2) + ┤ + "\n"

  def seperator(left: String, bridge: String, right: String, bridgeAt: Int): String = {
    val rest = ─ * (width - bridgeAt - 5)
    val overNumbers = ─ * (bridgeAt + 2)
    left + overNumbers + bridge + rest + right + "\n"
  }

  def makeHeader(text: String): String = {
    val x = width - text.charCount - 2
    val space = " " * (x / 2)
    val left = space
    val right = if (x % 2 == 0) space else space + " "

    top + │ + left + text + right + │ + "\n"
  }

  def makeLines(line: String, width: Int = width): String = {
    wordWrapper(line, width - 4).map(makeLine(_, width)).mkString
  }

  def makeLine(line: String, width: Int = width): String = {
    val whitespaces = " " * (width - line.charCount - 4)
    │ + " " + line + whitespaces + " " + │ + "\n"
  }

  def makeBlock(block: String): String = {
    val sb = new StringBuilder
    sb ++= divider
    sb ++= makeLines(block)
    sb.toString()
  }

  def makeBox(header: String, blocks: List[String]): String = {
    val sb = new StringBuilder
    sb ++= makeHeader(header)
    blocks foreach {sb ++= makeBlock(_)}
    sb ++= bottom
    sb.toString
  }
}

object SimpleFormatting extends Formatting(Simple, 80, new Colorizer(useColor = false))

object Boxes {

  val DefaultBox: Box = Light

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

    // Drop right to remove $ at end of object class name
    val name: String = getClass.getSimpleName.dropRight(1).toLowerCase
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

  lazy val All: Set[Box] = Enumeration.instancesOf[Box]
}
