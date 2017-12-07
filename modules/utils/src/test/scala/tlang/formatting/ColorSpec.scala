package tlang.formatting

import tlang.formatting.Colors._
import tlang.testutils.UnitSpec

class ColorSpec extends UnitSpec {


  it should "produce standard colors" in {
    NoColor.toString should matchWithAnsi("")
    Reset.toString should matchWithAnsi("\u001b[0m")
    Bold.toString should matchWithAnsi("\u001b[1m")
    Underline.toString should matchWithAnsi("\u001b[4m")
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
    Black("ABC") should matchWithAnsi("\u001b[30mABC\u001b[0m")
    Red("ABC") should matchWithAnsi("\u001b[31mABC\u001b[0m")
    Red("") should matchWithAnsi("")
  }


  it should "color any value" in {
    test("With normal toString") {
      val any = new Object {override def toString = "ABCDEFG" }

      Black(any) should matchWithAnsi("\u001b[30mABCDEFG\u001b[0m")
      Red(any) should matchWithAnsi("\u001b[31mABCDEFG\u001b[0m")
      Red + any should matchWithAnsi("\u001b[31mABCDEFG")
    }

    test("With empty toString") {
      val any = new Object {override def toString = "" }
      Black(any) should matchWithAnsi("")
    }
  }


  it should "be implicitly convertible to string" in {
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
    Red + "ABC" + Green + "DEF" + Yellow + "GHI" + Reset should matchWithAnsi(
      "\u001b[31mABC\u001b[32mDEF\u001b[33mGHI\u001b[0m"
    )
  }


  it should "be combined with each other" in {
    val color = Red + Underline + Bold + GreenBG
    color.toString should matchWithAnsi("\u001b[1;4;31;42m")
    color("ABC") should matchWithAnsi("\u001b[1;4;31;42mABC\u001b[0m")

    Red + "ABC" + color + "DEF" + Reset should matchWithAnsi("\u001b[31mABC\u001b[1;4;31;42mDEF\u001b[0m")

    Bold + Bold shouldBe Bold
  }

  it should "be removed from an other color" in {
    val color = Red + Underline + Bold + GreenBG
    color - Red shouldBe Underline + Bold + GreenBG
    color - (Red + Underline) shouldBe Bold + GreenBG
    color - (Red + Underline + Bold) shouldBe GreenBG
    color - (Red + Underline + Bold + GreenBG) shouldBe NoColor

    NoColor - Red shouldBe NoColor
    Red - NoColor shouldBe Red
  }


  it should "use the last foreground or background color added" in {
    val color = Red + RedBG + Underline + Bold + Green + GreenBG + Yellow + YellowBG
    color.toString should matchWithAnsi("\u001b[1;4;33;43m")
  }


  it should "adding reset should result in no color" in {
    val color = Red + RedBG + Underline + Bold + Green + GreenBG + Yellow + YellowBG + Reset
    color.toString should matchWithAnsi("")
    color shouldBe NoColor
  }


  it should "be created from an ansi escape sequence" in {
    Color("\u001b[31m").toString shouldBe "\u001b[31m"
    Color("\u001b[42;4;31;1m").toString shouldBe "\u001b[1;4;31;42m"
    Color("42;4;31;1").toString shouldBe "\u001b[1;4;31;42m"
    Color("31").toString shouldBe "\u001b[31m"

    intercept[IllegalArgumentException] { Color("abc") }
      .getMessage should include("abc")

    intercept[IllegalArgumentException] { Color("1;abc;32") }
      .getMessage should include("1;abc;32")

    intercept[IllegalArgumentException] { Color("abc\u001b[31mdef") }
      .getMessage should include("abc\u001b[31mdef")
  }


  it should "have correct color type" in {
    Color(RED).colorType shouldBe ColorType.Foreground
    Color(GREEN).colorType shouldBe ColorType.Foreground
    Red.colorType shouldBe ColorType.Foreground

    Color(RED_BG).colorType shouldBe ColorType.Background
    GreenBG.colorType shouldBe ColorType.Background

    NoColor.colorType shouldBe ColorType.NoColor
    Reset.colorType shouldBe ColorType.Reset

    Reset.colorType shouldBe ColorType.Reset

    Color(BOLD).colorType shouldBe ColorType.Modifier
    Bold.colorType shouldBe ColorType.Modifier

    (Bold + Underline).colorType shouldBe ColorType.Modifier

    (Bold + Green).colorType shouldBe ColorType.Mixed
    (RedBG + Green).colorType shouldBe ColorType.Mixed

    (RedBG + Green + Bold + Underline).colorType shouldBe ColorType.Mixed

    (Green + Reset).colorType shouldBe ColorType.NoColor
  }


  it should "produce correct string with colors" in {
    val BoldGreen = Bold + Green

    test("Empty") {
      val (str, colors) = Colors.splitStringAndColors("")
      str shouldBe ""
      colors shouldBe Array()
    }

    test("Only an ansi sequence") {
      val (str, colors) = Colors.splitStringAndColors("\u001b[31m")
      str shouldBe ""
      colors shouldBe Array()
    }

    test("String with colors") {
      val (str, colors) = Colors.splitStringAndColors(
        "abc\u001b[31mdef   ghi \u001b[1;32m def"
      )
      str shouldBe "abcdef   ghi  def"
      colors shouldBe Array(
        NoColor, //   'a'
        NoColor, //   'b'
        NoColor, //   'c'
        Red, //       'd'
        Red, //       'e'
        Red, //       'f'
        Red, //       ''
        Red, //       ''
        Red, //       ''
        Red, //       'g'
        Red, //       'h'
        Red, //       'i'
        Red, //       ''
        BoldGreen, // ''
        BoldGreen, // 'd'
        BoldGreen, // 'e'
        BoldGreen //  'f'
      )
    }

    test("Ansi escape sequences next to eachother and ending in ansi sequence") {
      val (str, colors) = Colors.splitStringAndColors(
        "\u001b[31m\u001b[32mabc\u001b[31mdef\u001b[0m   \u001b[1mghi\u001b[0m \u001b[1;32m def\u001b[0m"
      )
      str shouldBe "abcdef   ghi  def"
      colors shouldBe Array(
        Green, //     'a'
        Green, //     'b'
        Green, //     'c'
        Red, //       'd'
        Red, //       'e'
        Red, //       'f'
        NoColor, //   ''
        NoColor, //   ''
        NoColor, //   ''
        Bold, //      'g'
        Bold, //      'h'
        Bold, //      'i'
        NoColor, //   ''
        BoldGreen, // ''
        BoldGreen, // 'd'
        BoldGreen, // 'e'
        BoldGreen //  'f'
      )
    }

  }


  it should "determine whether a reset is needed between two colors" in {
    Red needsResetBefore NoColor shouldBe true
    NoColor needsResetBefore Red shouldBe false

    Red needsResetBefore Red shouldBe false
    Red needsResetBefore Green shouldBe false
    Red needsResetBefore Bold shouldBe true
    RedBG needsResetBefore Red shouldBe true
    RedBG needsResetBefore RedBG shouldBe false
    Bold needsResetBefore (Bold + Green) shouldBe false
    Bold needsResetBefore (Bold + Underline) shouldBe false
    GreenBG needsResetBefore (GreenBG + Red + Bold) shouldBe false
    (GreenBG + Red + Bold) needsResetBefore GreenBG shouldBe true
    (Red + Bold) needsResetBefore GreenBG shouldBe true
    (Red + Bold) needsResetBefore GreenBG shouldBe true

    (GreenBG + Red + Bold) needsResetBefore (GreenBG + Red) shouldBe true
    (GreenBG + Red + Bold) needsResetBefore (GreenBG + Red + Bold) shouldBe false
    (GreenBG + Red + Bold) needsResetBefore (GreenBG + Red + Bold + Underline) shouldBe false
    (GreenBG + Red) needsResetBefore (Red + Bold) shouldBe true
    (GreenBG + Red) needsResetBefore Bold shouldBe true
  }


  it should "throw when using unsupported color values" in {
    intercept[IllegalArgumentException] { Color(-5) }
      .getMessage should include("-5")

    intercept[IllegalArgumentException] { Color(70) }
      .getMessage should include("70")

    intercept[IllegalArgumentException] { Color(1337) }
      .getMessage should include("1337")
  }

}
