package tlang.formatting

import tlang.formatting.textformatters.TabReplacer
import tlang.testutils.UnitSpec
import tlang.utils.Extensions._
import tlang.utils.Position

class TabReplacerSpec extends UnitSpec {

  behavior of "A Tab replacer"


  it should "replace tabs and adjust given positions accordingly" in {

    test("Tab width 2") {
      val tabReplacer = TabReplacer(2)

      tabReplacer.apply("ABCDEF", Position(1, 3, 1, 5)) shouldBe("ABCDEF", Position(1, 3, 1, 5))

      tabReplacer.apply("\t\tABC\t\tDEF", Position(1, 1, 1, 1)) shouldBe("    ABC    DEF", Position(1, 1, 1, 1))
      tabReplacer.apply("\t\tABC\t\tDEF", Position(1, 2, 1, 2)) shouldBe("    ABC    DEF", Position(1, 3, 1, 3))
      tabReplacer.apply("\t\tABC\t\tDEF", Position(1, 2, 1, 6)) shouldBe("    ABC    DEF", Position(1, 3, 1, 8))

      tabReplacer.apply("\t\tABC\t\tDEF", Position(1, 1, 1, 11)) shouldBe("    ABC    DEF", Position(1, 1, 1, 15))
      tabReplacer.apply("\t\tABC\t\tDEF", Position(1, 8, 1, 9)) shouldBe("    ABC    DEF", Position(1, 12, 1, 13))
    }

    test("Tab width 4") {
      val tabReplacer = TabReplacer(4)

      tabReplacer.apply("\t\tABC\t\tDEF", Position(1, 1, 1, 1)) shouldBe("        ABC        DEF", Position(1, 1, 1, 1))
      tabReplacer.apply("\t\tABC\t\tDEF", Position(1, 2, 1, 2)) shouldBe("        ABC        DEF", Position(1, 5, 1, 5))
      tabReplacer.apply("\t\tABC\t\tDEF", Position(1, 2, 1, 6)) shouldBe("        ABC        DEF", Position(1, 5, 1, 12))

      tabReplacer.apply("\t\tABC\t\tDEF", Position(1, 1, 1, 11)) shouldBe("        ABC        DEF", Position(1, 1, 1, 23))
      tabReplacer.apply("\t\tABC\t\tDEF", Position(1, 8, 1, 9)) shouldBe("        ABC        DEF", Position(1, 20, 1, 21))
    }

  }


  it should "replace tabs and adjust multiple positions" in {

    val tabReplacer = TabReplacer(2)

    val positions = List(
      Position(1, 1, 1, 1),
      Position(1, 2, 1, 2),
      Position(1, 2, 1, 6),
      Position(1, 1, 1, 11),
      Position(1, 8, 1, 9),

      Position(2, 1, 2, 1),
      Position(2, 2, 2, 2),
      Position(2, 2, 2, 6),
      Position(2, 1, 2, 13),
      Position(2, 8, 2, 12)
    )

    val (adjustedText, adjustedPositions) = tabReplacer.apply("\t\tABC\t\tDEF" + NL + "ABC\tDEF\t\tGHI", positions)

    adjustedText shouldBe "    ABC    DEF" + NL + "ABC  DEF    GHI"
    adjustedPositions shouldBe List(
      Position(1, 1, 1, 1),
      Position(1, 3, 1, 3),
      Position(1, 3, 1, 8),
      Position(1, 1, 1, 15),
      Position(1, 12, 1, 13),

      Position(2, 1, 2, 1),
      Position(2, 2, 2, 2),
      Position(2, 2, 2, 7),
      Position(2, 1, 2, 16),
      Position(2, 9, 2, 15)
    )
  }


}
