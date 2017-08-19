package tlang.formatting

import tlang.formatting.Colors._
import tlang.testutils.UnitSpec

class ColorSpec extends UnitSpec {

  it should "produce standard colors" in {
    Reset.toString should matchWithAnsi("\u001b[0m")
    Bold.toString should matchWithAnsi("\u001b[1m")
    Underlined.toString should matchWithAnsi("\u001b[4m")
    Inverse.toString should matchWithAnsi("\u001b[7m")

    Black.toString should matchWithAnsi("\u001b[30m")
    Red.toString should matchWithAnsi("\u001b[31m")
    Green.toString should matchWithAnsi("\u001b[32m")
    Yellow.toString should matchWithAnsi("\u001b[33m")
    Blue.toString should matchWithAnsi("\u001b[34m")
    Magenta.toString should matchWithAnsi("\u001b[35m")
    Cyan.toString should matchWithAnsi("\u001b[36m")
    White.toString should matchWithAnsi("\u001b[37m")

    BlackBG.toString should matchWithAnsi("\u001b[40m")
    RedBG.toString should matchWithAnsi("\u001b[41m")
    GreenBG.toString should matchWithAnsi("\u001b[42m")
    YellowBG.toString should matchWithAnsi("\u001b[43m")
    BlueBG.toString should matchWithAnsi("\u001b[44m")
    MagentaBG.toString should matchWithAnsi("\u001b[45m")
    CyanBG.toString should matchWithAnsi("\u001b[46m")
    WhiteBG.toString should matchWithAnsi("\u001b[47m")
  }

  it should "color strings" in {
    Black()("ABC") should matchWithAnsi("\u001b[30mABC\u001b[0m")
    Red()("ABC") should matchWithAnsi("\u001b[31mABC\u001b[0m")
  }

  it should "color any" in {
    val anyClass = new Object {override def toString = "ABCDEFG" }

    Black()(anyClass) should matchWithAnsi("\u001b[30mABCDEFG\u001b[0m")
    Red()(anyClass) should matchWithAnsi("\u001b[31mABCDEFG\u001b[0m")
  }

  it should "be implicitly convertable to string" in {
    val sb = new StringBuilder
    sb ++= "ABC"
    sb ++= Red
    sb ++= "DEF"
    sb ++= Reset
    sb.toString should matchWithAnsi("ABC\u001b[31mDEF\u001b[0m")

    def assertColor(color: String) = color shouldBe "\u001b[31m"

    assertColor(Red)
  }

  it should "be added to strings" in {
    Red + "ABC" + Green + "DEF" + Yellow + "GHI" + Reset should matchWithAnsi("\u001b[31mABC\u001b[32mDEF\u001b[33mGHI\u001b[0m")
  }

  it should "not color strings when inactivated" in {
    Black(false)("ABC") should matchWithAnsi("ABC")
    Red(false)("ABC") should matchWithAnsi("ABC")

    Red(false).toString should matchWithAnsi("")
    Red(false) + "ABC" + Reset(false) should matchWithAnsi("ABC")
  }


  it should "be combined with each other" in {
    val color = Red + Underlined + Bold + GreenBG
    color.toString should matchWithAnsi("\u001b[1;4;31;42m")
    color("ABC") should matchWithAnsi("\u001b[1;4;31;42mABC\u001b[0m")

    Red + "ABC" + color + "DEF" + Reset should matchWithAnsi("\u001b[31mABC\u001b[1;4;31;42mDEF\u001b[0m")
  }

}
