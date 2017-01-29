package tcompiler.error

/**
  * Created by Tim Lindeberg on 1/29/2017.
  */

class BoxFormatting(chars: String) {
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
case object SimpleBoxFormatting        extends BoxFormatting("-|||||--||-")
case object DoubleBoxFormatting        extends BoxFormatting("═║╔╗╝╚╦╩╠╣╬")
case object LightBoxFormatting         extends BoxFormatting("─│┌┐┘└┬┴├┤┼")
case object HeavyBoxFormatting         extends BoxFormatting("━┃┏┓┛┗┳┻┣┫╋")
case object TripleDashBoxFormatting    extends BoxFormatting("┄┆┌┐┘└┬┴├┤┼")
case object QuadrupleDashBoxFormatting extends BoxFormatting("┈┊┌┐┘└┬┴├┤┼")
// @formatter:on