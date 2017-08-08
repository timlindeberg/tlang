package tlang.utils.formatting

import tlang.utils.Enumeration

object BoxStyles {

  val DefaultBox: BoxStyle = Light

  sealed abstract class BoxStyle(val chars: String) extends Product with Serializable {
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
    val name: String = getClass.getSimpleName.dropRight(1)
  }


  // @formatter:off
  case object Simple             extends BoxStyle("-|    --||+")
  case object NoLines            extends BoxStyle("           ")
  case object Double             extends BoxStyle("═║╔╗╝╚╦╩╠╣╬")
  case object Light              extends BoxStyle("─│┌┐┘└┬┴├┤┼")
  case object Heavy              extends BoxStyle("━┃┏┓┛┗┳┻┣┫╋")
  case object DoubleDashLight    extends BoxStyle("╌╎┌┐┘└┬┴├┤┼")
  case object DoubleDashHeavy    extends BoxStyle("╍╏┏┓┛┗┳┻┣┫╋")
  case object TripleDashLight    extends BoxStyle("┄┆┌┐┘└┬┴├┤┼")
  case object TripleDashHeavy    extends BoxStyle("┅┇┏┓┛┗┳┻┣┫╋")
  case object QuadrupleDashLight extends BoxStyle("┈┊┌┐┘└┬┴├┤┼")
  case object QuadrupleDashHeavy extends BoxStyle("┉┋┏┓┛┗┳┻┣┫╋")
  // @formatter:on

  lazy val All: List[BoxStyle] = Enumeration.instancesOf[BoxStyle]
}
