package tlang.formatting

import tlang.utils.Enumeration

object BoxStyles {

  val DefaultBox: BoxStyle = Unicode

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
    val ═ : String = chars(11).toString
    val ╒ : String = chars(12).toString
    val ╕ : String = chars(13).toString
    val ╘ : String = chars(14).toString
    val ╛ : String = chars(15).toString
    val ╞ : String = chars(16).toString
    val ╡ : String = chars(17).toString
    val ╤ : String = chars(18).toString
    val ╧ : String = chars(19).toString
    val ╪ : String = chars(20).toString

    // Drop right to remove $ at end of object class name
    val styleName: String = getClass.getSimpleName.dropRight(1)
  }

  // @formatter:off
  case object Ascii   extends BoxStyle("-|    --||+=    ||==+")
  case object NoLines extends BoxStyle("                     ")
  case object Unicode extends BoxStyle("─│┌┐┘└┬┴├┤┼═╒╕╘╛╞╡╤╧╪")
  // @formatter:on

  lazy val All: List[BoxStyle] = Enumeration.instancesOf[BoxStyle]

  lazy val Names: List[String] = All.map(_.styleName)
}
