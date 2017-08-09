package tlang.utils.formatting.grid

import org.scalatest.{FlatSpec, Matchers}
import tlang.utils.formatting.BoxStyles.Light
import tlang.utils.formatting.grid.Alignment.{Center, Left, Right}
import tlang.utils.formatting.grid.OverflowHandling.{Except, Truncate, Wrap}
import tlang.utils.formatting.grid.Width.{Fixed, Percentage}
import tlang.utils.formatting.{BoxStyles, Formatting}

class GridSpec extends FlatSpec with Matchers {

  val DefaultMaxWidth   = 20
  val DefaultFormatting = Formatting(Light, DefaultMaxWidth, useColor = false)

  behavior of "A Grid"

  it should "have correct grid size and attributes" in {
    val grid = Grid(DefaultFormatting)
      .row()
      .content("A")
      .row(Column, Column(width = Fixed(5)), Column(width = Percentage(0.2), alignment = Center, overflowHandling = Truncate))
      /**/ .content("A", "B", "C")
      /**/ .content("A", "B", "C")
      .row(alignment = Center)
      .row(Column())
      .row(Column, Column, Column, Column)
      .row(Column(), Column())
      /**/ .content("AB", "KL")
      /**/ .content("CD", "MN")
      /**/ .content(List("EF", "GH", "IJ"), List("OP", "QR", "ST"))

    grid should have size 6
    grid(0) should have size 1
    grid(0)(0).content shouldBe "A"
    grid(0)(0).width shouldBe Width.Auto
    grid(0)(0).overflowHandling shouldBe OverflowHandling.Wrap
    grid(0)(0).alignment shouldBe Alignment.Left

    grid(1) should have size 3
    grid(1)(0).content shouldBe "A\nA"
    grid(1)(0).width shouldBe Width.Auto
    grid(1)(0).overflowHandling shouldBe OverflowHandling.Wrap
    grid(1)(0).alignment shouldBe Alignment.Left

    grid(1)(1).content shouldBe "B\nB"
    grid(1)(1).width shouldBe Width.Fixed(5)
    grid(1)(1).overflowHandling shouldBe OverflowHandling.Wrap
    grid(1)(1).alignment shouldBe Alignment.Left

    grid(1)(2).content shouldBe "C\nC"
    grid(1)(2).width shouldBe Width.Percentage(0.2)
    grid(1)(2).overflowHandling shouldBe OverflowHandling.Truncate
    grid(1)(2).alignment shouldBe Alignment.Center

    grid(2) should have size 1
    grid(2)(0).content shouldBe ""
    grid(2)(0).overflowHandling shouldBe OverflowHandling.Wrap
    grid(2)(0).alignment shouldBe Alignment.Center


    grid(3) should have size 1
    grid(3)(0).content shouldBe ""

    grid(4) should have size 4
    grid(4)(0).content shouldBe ""

    grid(5) should have size 2
    grid(5)(0).content shouldBe "AB\nCD\nEF\nGH\nIJ"
    grid(5)(1).content shouldBe "KL\nMN\nOP\nQR\nST"
  }

  it should "be initialized with a default row" in {
    val grid = Grid(DefaultFormatting).content("ABC")
    grid should have size 1
    grid(0) should have size 1
    grid(0)(0).content shouldBe "ABC"
    grid(0)(0).width shouldBe Width.Auto
    grid(0)(0).alignment shouldBe Alignment.Left
  }

  it should "have correct column widths" in {
    // ┌──────────────────┐
    // │ A                │
    // └──────────────────┘
    Grid(DefaultFormatting)
      .row()
      .content("A")
      .columnWidths should contain theSameElementsInOrderAs Seq(16)

    // ┌──────────────────┐
    // │ A                │
    // └──────────────────┘
    Grid(DefaultFormatting)
      .row(Column(Width.Fixed(5)))
      .content("A")
      .columnWidths should contain theSameElementsInOrderAs Seq(16)

    // ┌───┬──────────────┐
    // │ A │ B            │
    // └───┴──────────────┘
    Grid(DefaultFormatting)
      .row(Column, Column)
      .content("A", "B")
      .columnWidths should contain theSameElementsInOrderAs Seq(1, 12)

    // ┌──────────────┬───┐
    // │ AB           │ C │
    // └──────────────┴───┘
    Grid(DefaultFormatting)
      .row(Column, Column)
      .content("AB", "C")
      .columnWidths should contain theSameElementsInOrderAs Seq(12, 1)

    // ┌───┬──────────────┐
    // │ A │ BC           │
    // └───┴──────────────┘
    Grid(DefaultFormatting)
      .row(Column, Column)
      .content("A", "BC")
      .columnWidths should contain theSameElementsInOrderAs Seq(1, 12)

    // ┌───┬───┬───┬──────┐
    // │ A │ B │ C │ D    │
    // └───┴───┴───┴──────┘
    Grid(DefaultFormatting)
      .row(Column, Column, Column, Column)
      .content("A", "B", "C", "D")
      .columnWidths should contain theSameElementsInOrderAs Seq(1, 1, 1, 4)

    // ┌────────┬─────────┐
    // │ ABC    │ DEF     │
    // └────────┴─────────┘
    Grid(DefaultFormatting)
      .row(Column(Width.Percentage(0.5)), Column)
      .content("ABC", "DEF")
      .columnWidths should contain theSameElementsInOrderAs Seq(6, 7)

    // ┌──────────┬───────┐
    // │ ABC      │ D     │
    // └──────────┴───────┘
    Grid(DefaultFormatting)
      .row(Column, Column(Width.Fixed(5)))
      .content("ABC", "D")
      .columnWidths should contain theSameElementsInOrderAs Seq(8, 5)


    // ┌────┬───────┬─────┐
    // │ AB │ D     │ DE  │
    // └────┴───────┴─────┘
    Grid(DefaultFormatting)
      .row(Column, Column(Width.Fixed(5)), Column)
      .content("AB", "C", "DE")
      .columnWidths should contain theSameElementsInOrderAs Seq(2, 5, 3)

    // ┌─────┬─────┬──────┐
    // │ A   │ B   │ C    │
    // └─────┴─────┴──────┘
    Grid(DefaultFormatting)
      .row(Column(Width.Percentage(0.33)), Column(Width.Percentage(0.33)), Column(Width.Percentage(0.33)))
      .content("A", "B", "C")
      .columnWidths should contain theSameElementsInOrderAs Seq(3, 3, 4)

    // ┌──────┬──────┬────┐
    // │ A    │ B    │ C  │
    // └──────┴──────┴────┘
    Grid(DefaultFormatting)
      .row(Column(Width.Fixed(4)), Column(Width.Fixed(4)), Column)
      .content("A", "B", "C")
      .columnWidths should contain theSameElementsInOrderAs Seq(4, 4, 2)

    // ┌───────┬──────┬───┐
    // │ A     │ BCD  │ E │
    // └───────┴──────┴───┘
    Grid(DefaultFormatting)
      .row(Column(Width.Fixed(5)), Column, Column)
      .content("A", "BCD", "E")
      .columnWidths should contain theSameElementsInOrderAs Seq(5, 4, 1)

    // ┌────────┬─────────┐
    // │ ABCDEF │ ABCDEFG │
    // │ GHIJKL │ HIJKLMN │
    // │ MNO    │ O       │
    // └────────┴─────────┘
    Grid(DefaultFormatting)
      .row(Column, Column)
      .content("ABCDEFGHIJKLMNO", "ABCDEFGHIJKLMNO")
      .columnWidths should contain theSameElementsInOrderAs Seq(6, 7)

    // ┌────┬───────┬─────┐
    // │ AB │ 12345 │ CDE │
    // ├────┴┬──────┼─────┤
    // │ ABC │ 1234 │ DEF │
    // └─────┴──────┴─────┘
    val grid = Grid(DefaultFormatting)
      .row(Column, Column, Column)
      .content("AB", "12345", "CDE")
      .row(Column, Column, Column)
      .content("ABC", "1234", "DEF")

    grid(0).columnWidths should contain theSameElementsInOrderAs Seq(2, 5, 3)
    grid(1).columnWidths should contain theSameElementsInOrderAs Seq(3, 4, 3)
  }

  it should "render correctly with a single row" in {
    Grid(DefaultFormatting)
      .row()
      .content("ABC")
      .toString shouldBe
      """|┌──────────────────┐
         |│ ABC              │
         |└──────────────────┘""".stripMargin.trim
  }

  it should "render correctly with a centered text" in {
    Grid(DefaultFormatting)
      .row(Column(alignment = Center))
      .content("ABC")
      .toString shouldBe
      """|┌──────────────────┐
         |│       ABC        │
         |└──────────────────┘""".stripMargin.trim
  }

  it should "render correctly with a right aligned text" in {
    Grid(DefaultFormatting)
      .row(Column(alignment = Right))
      .content("ABC")
      .toString shouldBe
      """|┌──────────────────┐
         |│              ABC │
         |└──────────────────┘""".stripMargin.trim
  }

  it should "render correctly with two columns" in {
    Grid(DefaultFormatting)
      .row(Column, Column)
      .content("ABC", "DEF")
      .toString shouldBe
      """|┌─────┬────────────┐
         |│ ABC │ DEF        │
         |└─────┴────────────┘""".stripMargin.trim
  }

  it should "render correctly with two columns and fixed width and centered text" in {
    Grid(DefaultFormatting)
      .row(Column(Width.Fixed(3)), Column(alignment = Alignment.Center))
      .content("ABC", "DEF")
      .toString shouldBe
      """|┌─────┬────────────┐
         |│ ABC │    DEF     │
         |└─────┴────────────┘""".stripMargin
  }

  it should "render correctly with multiple rows and columns" in {
    Grid(DefaultFormatting)
      .row(Column, Column, Column)
      .content("AB", "12345", "CDE")
      .row(Column, Column, Column)
      .content("ABC", "1234", "DEF")
      .toString shouldBe
      """|┌────┬───────┬─────┐
         |│ AB │ 12345 │ CDE │
         |├────┴┬──────┼─────┤
         |│ ABC │ 1234 │ DEF │
         |└─────┴──────┴─────┘""".stripMargin

    Grid(DefaultFormatting)
      .row(Column, Column)
      .content("ABC", "DEF")
      .row(Column, Column)
      .content("ABC", "DEF")
      .toString shouldBe
      """|┌─────┬────────────┐
         |│ ABC │ DEF        │
         |├─────┼────────────┤
         |│ ABC │ DEF        │
         |└─────┴────────────┘""".stripMargin
  }

  it should "render correctly with fixed and auto column widths" in {
    Grid(DefaultFormatting)
      .row(Column, Column(width = Fixed(2)), Column)
      .content("ABCDEFG", "12345", "HIJKMLN")
      .row(Column, Column, Column(width = Fixed(2)))
      .content("ABC", "12345678", "DEFGH")
    """|┌──────┬────┬──────┐
       |│ ABCD │ 12 │ HIJK │
       |│ EFG  │ 34 │ LMN  │
       |│      │ 5  │      │
       |├─────┬┴────┴─┬────┤
       |│ ABC │ 12345 │ DE │
       |│     │ 678   │ FG │
       |│     │       │ H  │
       |└─────┴───────┴────┘""".stripMargin
  }

  it should "render correctly with different indentation" in {
    Grid(DefaultFormatting.copy(lineWidth = 30))
      .indent(5)
      .row(Column, Column)
      .content("ABC", "DEF")
      .row(Column, Column)
      .content("ABC", "DEF")
      .toString shouldBe
      """|┌─────────────┬──────────────┐
         |│     ABC     │     DEF      │
         |├─────────────┼──────────────┤
         |│     ABC     │     DEF      │
         |└─────────────┴──────────────┘""".stripMargin
  }

  it should "render correctly with different box styles" in {
    val grid = Grid(DefaultFormatting)
      .row(Column, Column)
      .content("ABC", "DEF")
      .row(Column, Column)
      .content("ABC", "DEF")

    grid.toString shouldBe
      """|┌─────┬────────────┐
         |│ ABC │ DEF        │
         |├─────┼────────────┤
         |│ ABC │ DEF        │
         |└─────┴────────────┘""".stripMargin

    grid.formatting(DefaultFormatting.copy(box = BoxStyles.NoLines)).toString shouldBe
      """|
         |  ABC   DEF
         |
         |  ABC   DEF
         |""".stripMargin

    grid.formatting(DefaultFormatting.copy(box = BoxStyles.Double)).toString shouldBe
      """|╔═════╦════════════╗
         |║ ABC ║ DEF        ║
         |╠═════╬════════════╣
         |║ ABC ║ DEF        ║
         |╚═════╩════════════╝""".stripMargin

    grid.formatting(DefaultFormatting.copy(box = BoxStyles.Simple)).toString shouldBe
      """| ------------------
         || ABC | DEF        |
         ||-----+------------|
         || ABC | DEF        |
         | ------------------""".stripMargin
  }


  it should "render correctly with ansi colors" in {
    val formatting = DefaultFormatting.copy(useColor = true)
    Grid(formatting)
      .row(Column, Column)
      .content("\u001b[31mABC\u001b[0m", "\u001b[32mDEF\u001b[0m")
      .row(Column, Column)
      .content("A\u001b[1mB\u001b[0mC", "\u001b[4mD\u001b[0mEF")
      .toString shouldBe
      s"""|┌─────┬────────────┐
          |│ \u001b[31mABC\u001b[0m │ \u001b[32mDEF\u001b[0m        │
          |├─────┼────────────┤
          |│ A\u001b[1mB\u001b[0mC │ \u001b[4mD\u001b[0mEF        │
          |└─────┴────────────┘""".stripMargin


    Grid(formatting)
      .row()
      .content("\u001b[31mABCDEFGHIJKLMNOPQRSTUVXYZ\u001b[0m")
      .toString shouldBe
      s"""|┌──────────────────┐
          |│ \u001b[31mABCDEFGHIJKLMNOP\u001b[0m │
          |│ \u001b[31mQRSTUVXYZ\u001b[0m        │
          |└──────────────────┘""".stripMargin
  }

  it should "render correctly with line wrapping" in {
    Grid(DefaultFormatting)
      .row(Column, Column, Column)
      .content("ABCDEFGHIJKLMNOPQRSTUVXYZ", "ABCDEFGHIJKLMN", "ABCDEF")
      .row(Column, Column, Column)
      .content("ABCDEFGHIJ", "ABCDEFGHIJKLMNOPQRS", "ABCD")
      .content("ABCDEFGHIJKLMNOP", "ABCDEFG", "ABCDEFGHIJKLMNOPQRSTUVXYZ")
      .toString shouldBe
      """|┌──────┬─────┬─────┐
         |│ ABCD │ ABC │ ABC │
         |│ EFGH │ DEF │ DEF │
         |│ IJKL │ GHI │     │
         |│ MNOP │ JKL │     │
         |│ QRST │ MN  │     │
         |│ UVXY │     │     │
         |│ Z    │     │     │
         |├─────┬┴────┬┴─────┤
         |│ ABC │ ABC │ ABCD │
         |│ DEF │ DEF │      │
         |│ GHI │ GHI │      │
         |│ J   │ JKL │      │
         |│     │ MNO │      │
         |│     │ PQR │      │
         |│     │ S   │      │
         |│ ABC │ ABC │ ABCD │
         |│ DEF │ DEF │ EFGH │
         |│ GHI │ G   │ IJKL │
         |│ JKL │     │ MNOP │
         |│ MNO │     │ QRST │
         |│ P   │     │ UVXY │
         |│     │     │ Z    │
         |└─────┴─────┴──────┘""".stripMargin

    Grid(DefaultFormatting.copy(lineWidth = 21))
      .row(Column, Column, Column, Column, Column)
      .content("ABCDEFGHIJKL", "ABCDEFGHI", "ABCDEF", "ABC", "A")
      .toString shouldBe
      """|┌───┬───┬───┬───┬───┐
         |│ A │ A │ A │ A │ A │
         |│ B │ B │ B │ B │   │
         |│ C │ C │ C │ C │   │
         |│ D │ D │ D │   │   │
         |│ E │ E │ E │   │   │
         |│ F │ F │ F │   │   │
         |│ G │ G │   │   │   │
         |│ H │ H │   │   │   │
         |│ I │ I │   │   │   │
         |│ J │   │   │   │   │
         |│ K │   │   │   │   │
         |│ L │   │   │   │   │
         |└───┴───┴───┴───┴───┘""".stripMargin
  }

  it should "render correctly with empty columns" in {
    Grid(DefaultFormatting)
      .row(Column, Column)
      .content("", "ABC")
      .toString shouldBe
      """|┌───┬──────────────┐
         |│   │ ABC          │
         |└───┴──────────────┘""".stripMargin.trim

    Grid(DefaultFormatting)
      .row(Column, Column, Column, Column)
      .content("", "ABC", "", "")
      .row()
      .row(Column, Column)
      .content("", "")
      .row(Column, Column, Column, Column)
      .toString shouldBe
      """|┌───┬──────┬───┬───┐
         |│   │ ABC  │   │   │
         |├───┴──────┴───┴───┤
         |│                  │
         |├───┬──────────────┤
         |│   │              │
         |├───┼───┬───┬──────┤
         |│   │   │   │      │
         |└───┴───┴───┴──────┘""".stripMargin.trim
  }

  it should "render correctly with line wrapping and ansi colors" in {
    Grid(DefaultFormatting)
      .row(Column, Column, Column)
      .content("\u001b[31mABCD\u001b[32mEFGH\u001b[33mIJKL\u001b[34mMNOP\u001b[35mQRST\u001b[36mUVXY\u001b[37mZ\u001b[0m",
        "\u001b[31mABCD\u001b[32mEFGH\u001b[33mIJKL\u001b[34mMN\u001b[0m",
        "AB\u001b[31mCDE\u001b[0mF"
      )
      .toString shouldBe
      s"""|┌──────┬─────┬─────┐
          |│ \u001b[31mABCD\u001b[0m │ \u001b[31mABC\u001b[0m │ AB\u001b[31mC\u001b[0m │
          |│ \u001b[32mEFGH\u001b[0m │ \u001b[31mD\u001b[32mEF\u001b[0m │ \u001b[31mDE\u001b[0mF │
          |│ \u001b[33mIJKL\u001b[0m │ \u001b[32mGH\u001b[33mI\u001b[0m │     │
          |│ \u001b[34mMNOP\u001b[0m │ \u001b[33mJKL\u001b[0m │     │
          |│ \u001b[35mQRST\u001b[0m │ \u001b[34mMN\u001b[0m  │     │
          |│ \u001b[36mUVXY\u001b[0m │     │     │
          |│ \u001b[37mZ\u001b[0m    │     │     │
          |└──────┴─────┴─────┘""".stripMargin
  }

  it should "render correctly with truncation" in {
    Grid(DefaultFormatting)
      .row(Column(overflowHandling = Truncate), Column(overflowHandling = Truncate), Column(overflowHandling = Truncate))
      .content("ABCDEFGHIJKLMNOPQRSTUVXYZ", "ABCDEFGHIJKLMN", "AB")
      .row(Column(overflowHandling = Truncate), Column(overflowHandling = Truncate), Column(overflowHandling = Truncate))
      .content("ABC", "ABCDEFGHIJKLMNOPQRS", "ABCD")
      .content("ABCDEFGHIJKLMNOP", "ABC", "ABCDEFGHIJKLMNOPQRSTUVXYZ")
      .toString shouldBe
      """|┌──────┬──────┬────┐
         |│ A... │ A... │ AB │
         |├─────┬┴────┬─┴────┤
         |│ ABC │ ... │ ABCD │
         |│ ... │ ABC │ A... │
         |└─────┴─────┴──────┘""".stripMargin
  }

  it should "render correctly with truncation and ansi colors" in {
    Grid(DefaultFormatting)
      .row(Column(overflowHandling = Truncate), Column(overflowHandling = Truncate), Column(overflowHandling = Truncate))
      .content("\u001b[31mABCDEFGHIJKLMNOPQRSTUVXYZ\u001b[0m", "ABCDEFGHIJKLMN", "\u001b[32mAB\u001b[0m")
      .row(Column(overflowHandling = Truncate), Column(overflowHandling = Truncate), Column(overflowHandling = Truncate))
      .content("ABC", "\u001b[33mABCDEFGHIJKLMNOPQRS\u001b[0m", "\u001b[34mABCD\u001b[0m")
      .content("ABCDEFGHIJKLMNOP", "ABC", "\u001b[35mABC\u001b[36mDEF\u001b[37mGHIJKLMNOPQRSTUVXYZ\u001b[0m")
      .toString shouldBe
      s"""|┌──────┬──────┬────┐
          |│ \u001b[31mA\u001b[0m... │ A... │ \u001b[32mAB\u001b[0m │
          |├─────┬┴────┬─┴────┤
          |│ ABC │ ... │ \u001b[34mABCD\u001b[0m │
          |│ ... │ ABC │ \u001b[35mA\u001b[0m... │
          |└─────┴─────┴──────┘""".stripMargin
  }

  it should "throw when given content with improper dimension" in {
    intercept[IllegalArgumentException] {
      Grid(DefaultFormatting)
        .row()
        .content("A", "B")

    }.getMessage should include("2 != 1")

    intercept[IllegalArgumentException] {
      Grid(DefaultFormatting)
        .row(Column, Column, Column)
        .content("A", "B")

    }.getMessage should include("2 != 3")

    intercept[IllegalArgumentException] {
      Grid(DefaultFormatting)
        .row(Column, Column, Column)
        .content(List("A", "B"), List("C", "D"))

    }.getMessage should include("2 != 3")
  }

  it should "throw when line doesn't fit in column with OverflowHandling.Except" in {
    intercept[IllegalStateException] {
      Grid(DefaultFormatting)
        .row(Column(overflowHandling = Except))
        .content("ABCDEFGHIJKLMNOPQRSTUVXYZ")
        .content("ABC")
        .toString

    }.getMessage should (include("25 > 16") and include("ABCDEFGHIJKLMNOPQRSTUVXYZ"))
  }

  it should "throw when columns cannot fit in the row" in {
    intercept[IllegalStateException] {
      Grid(DefaultFormatting).row(Column(width = Fixed(25)))

    }.getMessage should include("29 > 20")

    intercept[IllegalStateException] {
      Grid(DefaultFormatting).row(Column(width = Percentage(0.9)), Column, Column)

    }.getMessage should include("21 > 20")

    intercept[IllegalStateException] {
      Grid(DefaultFormatting).row(Column(width = Percentage(0.9)), Column(width = Fixed(3)))

    }.getMessage should include("21 > 20")

    intercept[IllegalStateException] {
      Grid(DefaultFormatting).row(Column, Column, Column, Column, Column)

    }.getMessage should include("21 > 20")

    intercept[IllegalStateException] {
      Grid(DefaultFormatting).row(Column(width = Fixed(5)), Column(width = Fixed(5)), Column)

    }.getMessage should include("21 > 20")
  }


  behavior of "Alignment"


  it should "left align correctly" in {
    Left("", 10) shouldBe "          "
    Left("ABC", 10) shouldBe "ABC       "
    Left("ABCDEFGHIJ", 10) shouldBe "ABCDEFGHIJ"
    Left("", 5) shouldBe "     "
    Left("ABC", 5) shouldBe "ABC  "

    Left("ABC", 10, '-') shouldBe "ABC-------"
  }

  it should "center correctly" in {
    Center("", 10) shouldBe "          "
    Center("AB", 10) shouldBe "    AB    "
    Center("ABC", 10) shouldBe "   ABC    "
    Center("ABCDEFGHIJ", 10) shouldBe "ABCDEFGHIJ"

    Center("", 5) shouldBe "     "
    Center("AB", 5) shouldBe " AB  "
    Center("ABC", 5) shouldBe " ABC "
    Center("ABCD", 5) shouldBe "ABCD "

    Center("ABC", 10, '-') shouldBe "---ABC----"
  }

  it should "right align correctly" in {
    Right("", 10) shouldBe "          "
    Right("ABC", 10) shouldBe "       ABC"
    Right("ABCDEFGHIJ", 10) shouldBe "ABCDEFGHIJ"

    Right("", 5) shouldBe "     "
    Right("ABC", 5) shouldBe "  ABC"

    Right("ABC", 10, '-') shouldBe "-------ABC"
  }

  it should "align correctly with ansi colors" in {
    val str = "A\u001b[31mB\u001b[0mC"
    Left(str, 10) shouldBe "A\u001b[31mB\u001b[0mC       "
    Center(str, 10) shouldBe "   A\u001b[31mB\u001b[0mC    "
    Right(str, 10) shouldBe "       A\u001b[31mB\u001b[0mC"
  }

  it should "throw when size is larger then width" in {
    intercept[IllegalArgumentException] { Left("ABC", 2) }.getMessage should include("3 > 2")
    intercept[IllegalArgumentException] { Center("ABCDE", 1) }.getMessage should include("5 > 1")
    intercept[IllegalArgumentException] { Right("ABCDEFGH", 7) }.getMessage should include("8 > 7")
  }

  it should "throw when given an invalid width" in {
    intercept[IllegalArgumentException] { Left("ABC", 0) }.getMessage should include("0")
    intercept[IllegalArgumentException] { Left("ABC", -25) }.getMessage should include("-25")
  }


  behavior of "Overflow handlong"


  it should "truncate regular text" in {
    val text = "ABCDEFGHIJKLMNO" // 15 chars long
    Truncate(text, 1).head shouldBe "."
    Truncate(text, 2).head shouldBe ".."
    Truncate(text, 3).head shouldBe "..."
    Truncate(text, 4).head shouldBe "A..."
    Truncate(text, 5).head shouldBe "AB..."
    Truncate(text, 10).head shouldBe "ABCDEFG..."
    Truncate(text, 14).head shouldBe "ABCDEFGHIJK..."
    Truncate(text, 15).head shouldBe "ABCDEFGHIJKLMNO"
    Truncate(text, 16).head shouldBe "ABCDEFGHIJKLMNO"
  }

  it should "truncate ansi colored text" in {
    val text = "\u001b[32mABC\u001b[33mDEF\u001b[34mGHI\u001b[35mJKL\u001b[36mMNO\u001b[0m" // 15 real chars long
    Truncate(text, 1).head shouldBe "."
    Truncate(text, 2).head shouldBe ".."
    Truncate(text, 3).head shouldBe "..."
    Truncate(text, 4).head shouldBe "\u001b[32mA\u001b[0m..."
    Truncate(text, 5).head shouldBe "\u001b[32mAB\u001b[0m..."
    Truncate(text, 10).head shouldBe "\u001b[32mABC\u001b[33mDEF\u001b[34mG\u001b[0m..."
    Truncate(text, 14).head shouldBe "\u001b[32mABC\u001b[33mDEF\u001b[34mGHI\u001b[35mJK\u001b[0m..."
    Truncate(text, 15).head shouldBe "\u001b[32mABC\u001b[33mDEF\u001b[34mGHI\u001b[35mJKL\u001b[36mMNO\u001b[0m"
    Truncate(text, 15).head should be theSameInstanceAs text
    Truncate(text, 16).head shouldBe "\u001b[32mABC\u001b[33mDEF\u001b[34mGHI\u001b[35mJKL\u001b[36mMNO\u001b[0m"
    Truncate(text, 16).head should be theSameInstanceAs text
  }

  it should "throw when given an invalid width" in {
    intercept[IllegalArgumentException] { Except("ABC", 0) }.getMessage should include("0")
    intercept[IllegalArgumentException] { Wrap("ABC", -25) }.getMessage should include("-25")
    intercept[IllegalArgumentException] { Truncate("ABC", -1) }.getMessage should include("-1")
  }

  it should "throw when line doesn't fit with Except" in {
    intercept[IllegalStateException] {
      Except("ABCDEFGHIJKLMNOPQRSTUVXYZ", 20)

    }.getMessage should (include("25 > 20") and include("ABCDEFGHIJKLMNOPQRSTUVXYZ"))

  }

}
