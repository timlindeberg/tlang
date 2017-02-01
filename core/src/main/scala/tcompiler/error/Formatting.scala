package tcompiler.error

import tcompiler.utils.Colorizer

/**
  * Created by Tim Lindeberg on 1/29/2017.
  */

case class Formatting(boxType: BoxType, colorizer: Colorizer)

object SimpleFormatting extends Formatting(SimpleBox, new Colorizer(useColor = false))

abstract class BoxType(chars: String) {
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
}

// @formatter:off
case object SimpleBox             extends BoxType("-|||||--||-")
case object DoubleBox             extends BoxType("═║╔╗╝╚╦╩╠╣╬")
case object LightBox              extends BoxType("─│┌┐┘└┬┴├┤┼")
case object HeavyBox              extends BoxType("━┃┏┓┛┗┳┻┣┫╋")
case object DoubleDashLightBox    extends BoxType("╌╎┌┐┘└┬┴├┤┼")
case object DoubleDashHeavyBox    extends BoxType("╍╏┏┓┛┗┳┻┣┫╋")
case object TripleDashLightBox    extends BoxType("┄┆┌┐┘└┬┴├┤┼")
case object TripleDashHeavyBox    extends BoxType("┅┇┏┓┛┗┳┻┣┫╋")
case object QuadrupleDashLightBox extends BoxType("┈┊┌┐┘└┬┴├┤┼")
case object QuadrupleDashHeavyBox extends BoxType("┉┋┏┓┛┗┳┻┣┫╋")
// @formatter:on