package tlang.utils.formatting.grid

import org.scalatest.{FlatSpec, Matchers}
import tlang.testutils.AnsiMatchers._
import tlang.utils.formatting.BoxStyles.Unicode
import tlang.utils.formatting.grid.Alignment.{Center, Left, Right}
import tlang.utils.formatting.grid.OverflowHandling.{Except, Truncate, Wrap}
import tlang.utils.formatting.grid.Width.{Auto, Fixed, Percentage}
import tlang.utils.formatting.{BoxStyles, Formatting}

class GridSpec extends FlatSpec with Matchers {

  val DefaultMaxWidth   = 20
  val DefaultFormatting = Formatting(Unicode, DefaultMaxWidth, useColor = false)

  behavior of "A Grid"

  it should "have correct grid size and attributes" in {
    val threeColumns = List(Column(width = Fixed(5)), Column, Column(overflowHandling = Except))
    val grid = Grid(DefaultFormatting)
      .header("ABC")
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
      /**/ .contents(List(("EF", "OP"), ("GH", "QR"), ("IJ", "ST")))
      .row(threeColumns)
      .row(4)

    grid should have size 9

    grid(0) should have size 1
    grid(0).isHeader shouldBe true
    grid(0)(0).content shouldBe "ABC"
    grid(0)(0).width shouldBe Auto
    grid(0)(0).overflowHandling shouldBe Wrap
    grid(0)(0).alignment shouldBe Center

    grid(1) should have size 1
    grid(1)(0).content shouldBe "A"
    grid(1)(0).width shouldBe Auto
    grid(1)(0).overflowHandling shouldBe Wrap
    grid(1)(0).alignment shouldBe Left

    grid(2) should have size 3
    grid(2)(0).content shouldBe "A\nA"
    grid(2)(0).width shouldBe Auto
    grid(2)(0).overflowHandling shouldBe Wrap
    grid(2)(0).alignment shouldBe Left

    grid(2)(1).content shouldBe "B\nB"
    grid(2)(1).width shouldBe Fixed(5)
    grid(2)(1).overflowHandling shouldBe Wrap
    grid(2)(1).alignment shouldBe Left

    grid(2)(2).content shouldBe "C\nC"
    grid(2)(2).width shouldBe Percentage(0.2)
    grid(2)(2).overflowHandling shouldBe Truncate
    grid(2)(2).alignment shouldBe Center

    grid(3) should have size 1
    grid(3)(0).content shouldBe ""
    grid(3)(0).overflowHandling shouldBe Wrap
    grid(3)(0).alignment shouldBe Center


    grid(4) should have size 1
    grid(4)(0).content shouldBe ""

    grid(5) should have size 4
    grid(5)(0).content shouldBe ""

    grid(6) should have size 2
    grid(6)(0).content shouldBe "AB\nCD\nEF\nGH\nIJ"
    grid(6)(1).content shouldBe "KL\nMN\nOP\nQR\nST"

    grid(7) should have size 3
    grid(7)(0).width shouldBe Fixed(5)
    grid(7)(1).width shouldBe Auto
    grid(7)(2).overflowHandling shouldBe Except

    grid(8) should have size 4
    grid(8)(0).content shouldBe ""
    grid(8)(0).width shouldBe Auto
    grid(8)(1).alignment shouldBe Left
    grid(8)(2).overflowHandling shouldBe Wrap
  }

  it should "be initialized with a default row" in {
    val grid = Grid(DefaultFormatting).content("ABC")
    grid should have size 1
    grid(0) should have size 1
    grid(0)(0).content shouldBe "ABC"
    grid(0)(0).width shouldBe Width.Auto
    grid(0)(0).alignment shouldBe Alignment.Left
  }

  it should "have correct content" in {

    val sixTuple = ("A", "B", "C", "D", "E", "F")
    val grid = Grid(DefaultFormatting.copy(lineWidth = 10000))
      .row()
      .content("A")
      .row(Column, Column)
      .content("A", "B")
      .row(Column, Column)
      .content(("A", "B"))
      .row(Column, Column, Column, Column, Column, Column)
      .content(sixTuple)
      .row(Column, Column, Column, Column, Column, Column)
      .mapContent(0 to 3) { i => (i, i + 1, i + 2, i + 3, i + 4, i + 5) }
      .row(Column, Column, Column)
      .contents(Seq(("A", "D", "G"), ("B", "E", "H"), ("C", "F", "I")))
      .row(Column, Column)
      .mapContent(Seq("ABCDE", "EFGHI")) { s => (s.reverse, s) }
      .row(Column, Column, Column)
      .allContent(Seq(Seq("A", "B", "C"), Seq("D", "E", "F"), Seq("I", "J", "K")))
      .row(Column, Column, Column)
      .content()

    grid(0)(0).content shouldBe "A"

    grid(1)(0).content shouldBe "A"
    grid(1)(1).content shouldBe "B"

    grid(2)(0).content shouldBe "A"
    grid(2)(1).content shouldBe "B"

    grid(3)(0).content shouldBe "A"
    grid(3)(1).content shouldBe "B"
    grid(3)(2).content shouldBe "C"
    grid(3)(3).content shouldBe "D"
    grid(3)(4).content shouldBe "E"
    grid(3)(5).content shouldBe "F"

    grid(4)(0).content shouldBe "0\n1\n2\n3"
    grid(4)(1).content shouldBe "1\n2\n3\n4"
    grid(4)(2).content shouldBe "2\n3\n4\n5"
    grid(4)(3).content shouldBe "3\n4\n5\n6"
    grid(4)(4).content shouldBe "4\n5\n6\n7"
    grid(4)(5).content shouldBe "5\n6\n7\n8"

    grid(5)(0).content shouldBe "A\nB\nC"
    grid(5)(1).content shouldBe "D\nE\nF"
    grid(5)(2).content shouldBe "G\nH\nI"

    grid(6)(0).content shouldBe "EDCBA\nIHGFE"
    grid(6)(1).content shouldBe "ABCDE\nEFGHI"

    grid(5)(0).content shouldBe "A\nB\nC"
    grid(5)(1).content shouldBe "D\nE\nF"
    grid(5)(2).content shouldBe "G\nH\nI"
  }

  it should "be reset when calling clear" in {
    val grid = Grid(DefaultFormatting.copy(lineWidth = 10000))
      .row()
      .content("A")
      .row(Column, Column)
      .content("A", "B")
      .row(Column, Column)
      .content(("A", "B"))

    grid should have size 3
    grid(1) should have size 2

    grid.clear()

    grid should have size 0
    grid.toString shouldBe ""

    grid
      .row()
      .content("A")
      .row(2)
      .content("A", "B")

    grid should have size 2
    grid(1) should have size 2
    grid(1)(0).content shouldBe "A"
  }

  it should "have correct column widths" in {
    // ┌──────────────────┐
    // │ A                │
    // └──────────────────┘
    var grid = Grid(DefaultFormatting)
      .row()
      .content("A")

    grid(0).columnWidths should contain theSameElementsInOrderAs Seq(16)

    // ┌──────────────────┐
    // │ A                │
    // └──────────────────┘
    grid = Grid(DefaultFormatting)
      .row(Column(Width.Fixed(5)))
      .content("A")

    grid(0).columnWidths should contain theSameElementsInOrderAs Seq(16)

    // ┌───┬──────────────┐
    // │ A │ B            │
    // └───┴──────────────┘
    grid = Grid(DefaultFormatting)
      .row(Column, Column)
      .content("A", "B")


    grid(0).columnWidths should contain theSameElementsInOrderAs Seq(1, 12)

    // ┌────┬─────────────┐
    // │ AB │ C           │
    // └────┴─────────────┘
    grid = Grid(DefaultFormatting)
      .row(Column, Column)
      .content("AB", "C")

    grid(0).columnWidths should contain theSameElementsInOrderAs Seq(2, 11)

    // ┌───┬──────────────┐
    // │ A │ BC           │
    // └───┴──────────────┘
    grid = Grid(DefaultFormatting)
      .row(Column, Column)
      .content("A", "BC")

    grid(0).columnWidths should contain theSameElementsInOrderAs Seq(1, 12)

    // ┌───┬───┬───┬──────┐
    // │ A │ B │ C │ D    │
    // └───┴───┴───┴──────┘
    grid = Grid(DefaultFormatting)
      .row(Column, Column, Column, Column)
      .content("A", "B", "C", "D")

    grid(0).columnWidths should contain theSameElementsInOrderAs Seq(1, 1, 1, 4)

    // ┌────────┬─────────┐
    // │ ABC    │ DEF     │
    // └────────┴─────────┘
    grid = Grid(DefaultFormatting)
      .row(Column(Width.Percentage(0.5)), Column)
      .content("ABC", "DEF")

    grid(0).columnWidths should contain theSameElementsInOrderAs Seq(6, 7)

    // ┌──────────┬───────┐
    // │ ABC      │ D     │
    // └──────────┴───────┘
    grid = Grid(DefaultFormatting)
      .row(Column, Column(Width.Fixed(5)))
      .content("ABC", "D")

    grid(0).columnWidths should contain theSameElementsInOrderAs Seq(8, 5)


    // ┌────┬───────┬─────┐
    // │ AB │ D     │ DE  │
    // └────┴───────┴─────┘
    grid = Grid(DefaultFormatting)
      .row(Column, Column(Width.Fixed(5)), Column)
      .content("AB", "C", "DE")

    grid(0).columnWidths should contain theSameElementsInOrderAs Seq(2, 5, 3)

    // ┌─────┬─────┬──────┐
    // │ A   │ B   │ C    │
    // └─────┴─────┴──────┘
    grid = Grid(DefaultFormatting)
      .row(Column(Width.Percentage(0.33)), Column(Width.Percentage(0.33)), Column(Width.Percentage(0.33)))
      .content("A", "B", "C")

    grid(0).columnWidths should contain theSameElementsInOrderAs Seq(3, 3, 4)

    // ┌──────┬──────┬────┐
    // │ A    │ B    │ C  │
    // └──────┴──────┴────┘
    grid = Grid(DefaultFormatting)
      .row(Column(Width.Fixed(4)), Column(Width.Fixed(4)), Column)
      .content("A", "B", "C")

    grid(0).columnWidths should contain theSameElementsInOrderAs Seq(4, 4, 2)

    // ┌───────┬─────┬────┐
    // │ A     │ BCD │ E  │
    // └───────┴─────┴────┘
    grid = Grid(DefaultFormatting)
      .row(Column(Width.Fixed(5)), Column, Column)
      .content("A", "BCD", "E")

    grid(0).columnWidths should contain theSameElementsInOrderAs Seq(5, 3, 2)

    // ┌────────┬─────────┐
    // │ ABCDEF │ ABCDEFG │
    // │ GHIJKL │ HIJKLMN │
    // │ MNO    │ O       │
    // └────────┴─────────┘
    grid = Grid(DefaultFormatting)
      .row(Column, Column)
      .content("ABCDEFGHIJKLMNO", "ABCDEFGHIJKLMNO")

    grid(0).columnWidths should contain theSameElementsInOrderAs Seq(6, 7)

    // ┌────┬───────┬─────┐
    // │ AB │ 12345 │ CDE │
    // ├────┴┬──────┼─────┤
    // │ ABC │ 1234 │ DEF │
    // └─────┴──────┴─────┘
    grid = Grid(DefaultFormatting)
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

  it should "render correctly with a header" in {
    Grid(DefaultFormatting)
      .header("Header")
      .row(Column, Column, Column)
      .content("ABC", "DEFG", "HIJ")
      .toString shouldBe
      """|╒══════════════════╕
         |│      Header      │
         |╞═════╤══════╤═════╡
         |│ ABC │ DEFG │ HIJ │
         |└─────┴──────┴─────┘""".stripMargin

  }

  it should "render correctly with different box styles" in {
    val grid = Grid(DefaultFormatting)
      .header("Header")
      .row(Column, Column)
      .content("ABC", "DEF")
      .row(Column, Column)
      .content("ABC", "DEF")

    grid.toString shouldBe
      """|╒══════════════════╕
         |│      Header      │
         |╞═════╤════════════╡
         |│ ABC │ DEF        │
         |├─────┼────────────┤
         |│ ABC │ DEF        │
         |└─────┴────────────┘""".stripMargin

    grid.formatting(DefaultFormatting.copy(boxStyle = BoxStyles.NoLines)).toString shouldBe
      """|
         |       Header
         |
         |  ABC   DEF
         |
         |  ABC   DEF
         |""".stripMargin

    grid.formatting(DefaultFormatting.copy(boxStyle = BoxStyles.Ascii)).toString shouldBe
      """| ==================
         ||      Header      |
         ||==================|
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
      .toString should matchWithAnsi(
      s"""|┌──────────────────┐
          |│ \u001b[31mABCDEFGHIJKLMNOP\u001b[0m │
          |│ \u001b[31mQRSTUVXYZ\u001b[0m        │
          |└──────────────────┘""".stripMargin)
  }

  it should "render correctly with colored borders" in {
    val formatting = DefaultFormatting.copy(useColor = true)
    Grid(DefaultFormatting)
      .borderColor(formatting.Red)
      .row(Column, Column)
      .content("ABC", "DEF")
      .row(Column, Column)
      .content("ABC", "DEF")
      .toString should matchWithAnsi(
      """|\u001b[31m┌─────┬────────────┐\u001b[0m
         |\u001b[31m│\u001b[0m ABC \u001b[31m│\u001b[0m DEF        \u001b[31m│\u001b[0m
         |\u001b[31m├─────┼────────────┤\u001b[0m
         |\u001b[31m│\u001b[0m ABC \u001b[31m│\u001b[0m DEF        \u001b[31m│\u001b[0m
         |\u001b[31m└─────┴────────────┘\u001b[0m""".stripMargin)
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
      """|┌───┬─────┬───┬────┐
         |│   │ ABC │   │    │
         |├───┴─────┴───┴────┤
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
      .toString should matchWithAnsi(
      s"""|┌──────┬─────┬─────┐
          |│ \u001b[31mABCD\u001b[0m │ \u001b[31mABC\u001b[0m │ AB\u001b[31mC\u001b[0m │
          |│ \u001b[32mEFGH\u001b[0m │ \u001b[31mD\u001b[32mEF\u001b[0m │ \u001b[31mDE\u001b[0mF │
          |│ \u001b[33mIJKL\u001b[0m │ \u001b[32mGH\u001b[33mI\u001b[0m │     │
          |│ \u001b[34mMNOP\u001b[0m │ \u001b[33mJKL\u001b[0m │     │
          |│ \u001b[35mQRST\u001b[0m │ \u001b[34mMN\u001b[0m  │     │
          |│ \u001b[36mUVXY\u001b[0m │     │     │
          |│ \u001b[37mZ\u001b[0m    │     │     │
          |└──────┴─────┴─────┘""".stripMargin)
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
      .toString should matchWithAnsi(
      s"""|┌──────┬──────┬────┐
          |│ \u001b[31mA\u001b[0m... │ A... │ \u001b[32mAB\u001b[0m │
          |├─────┬┴────┬─┴────┤
          |│ ABC │ ... │ \u001b[34mABCD\u001b[0m │
          |│ ... │ ABC │ \u001b[35mA\u001b[0m... │
          |└─────┴─────┴──────┘""".stripMargin)
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
        .contents(List(("A", "B"), ("C", "D")))

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

  it should "throw when adding a header once a rows been added" in {
    intercept[IllegalStateException] {
      Grid(DefaultFormatting).row().header("Header")
    }

    intercept[IllegalStateException] {
      Grid(DefaultFormatting)
        .content("ABC")
        .header("Header")
    }

    intercept[IllegalStateException] {
      Grid(DefaultFormatting)
        .row(Column, Column, Column, Column)
        .content("", "ABC", "", "")
        .row()
        .row(Column, Column)
        .content("", "")
        .header("Header")
        .row(Column, Column, Column, Column)
    }
  }

  it should "throw when given contents other than a tuple" in {
    intercept[IllegalArgumentException] {
      Grid(DefaultFormatting)
        .contents(List(Nil))

    }.getMessage should include("Nil")

    // Column is a case class and there for implements Product
    intercept[IllegalArgumentException] {
      Grid(DefaultFormatting)
        .row(3)
        .contents(List(("1", "2", "3"), ("4", "5", "6"), Column))

    }.getMessage should include("Column")
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
    Left(str, 10) should matchWithAnsi("A\u001b[31mB\u001b[0mC       ")
    Center(str, 10) should matchWithAnsi("   A\u001b[31mB\u001b[0mC    ")
    Right(str, 10) should matchWithAnsi("       A\u001b[31mB\u001b[0mC")
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


  behavior of "Overflow handling"


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
    Truncate(text, 1).head should matchWithAnsi(".")
    Truncate(text, 2).head should matchWithAnsi("..")
    Truncate(text, 3).head should matchWithAnsi("...")
    Truncate(text, 4).head should matchWithAnsi("\u001b[32mA\u001b[0m...")
    Truncate(text, 5).head should matchWithAnsi("\u001b[32mAB\u001b[0m...")
    Truncate(text, 10).head should matchWithAnsi("\u001b[32mABC\u001b[33mDEF\u001b[34mG\u001b[0m...")
    Truncate(text, 14).head should matchWithAnsi("\u001b[32mABC\u001b[33mDEF\u001b[34mGHI\u001b[35mJK\u001b[0m...")
    Truncate(text, 15).head should matchWithAnsi("\u001b[32mABC\u001b[33mDEF\u001b[34mGHI\u001b[35mJKL\u001b[36mMNO\u001b[0m")
    Truncate(text, 15).head should be theSameInstanceAs text
    Truncate(text, 16).head should matchWithAnsi("\u001b[32mABC\u001b[33mDEF\u001b[34mGHI\u001b[35mJKL\u001b[36mMNO\u001b[0m")
    Truncate(text, 16).head should be theSameInstanceAs text

    Truncate("ABCDEF\u001b[32mGHIJ\u001b[0m", 9).head should matchWithAnsi("ABCDEF...")
    Truncate("ABCDEF\u001b[32mGHIJK\u001b[0m", 10).head should matchWithAnsi("ABCDEF\u001b[32mG\u001b[0m...")
    Truncate("\u001b[31mABCDEF\u001b[32mGHIJ\u001b[0m", 9).head should matchWithAnsi("\u001b[31mABCDEF\u001b[0m...")
    Truncate("\u001b[31mABCDEF\u001b[0mGHIJ", 9).head should matchWithAnsi("\u001b[31mABCDEF\u001b[0m...")
    Truncate("\u001b[31mABC\u001b[0mDEFGHIJ", 9).head should matchWithAnsi("\u001b[31mABC\u001b[0mDEF...")
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
