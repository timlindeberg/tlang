package tlang.utils.formatting

import org.scalatest.{FlatSpec, Matchers}
import tlang.utils.Extensions._
import tlang.utils.formatting.Alignment.{Center, Right}
import tlang.utils.formatting.BoxStyles.Light
import tlang.utils.formatting.OverflowHandling.{Except, Truncate}

class GridSpec extends FlatSpec with Matchers {

  val DefaultMaxWidth   = 20
  val DefaultFormatting = Formatting(Light, DefaultMaxWidth, useColor = false)

  behavior of "A Grid"

  it should "have correct grid size and attributes" in {
    val grid = Grid(DefaultFormatting)
      .row()
      .content("A")
      .row(Column(), Column(ColumnWidth.Fixed(5)), Column(ColumnWidth.Percentage(0.2), OverflowHandling.Truncate, Alignment.Center))
      /**/ .content("A", "B", "C")
      /**/ .content("A", "B", "C")
      .row(Alignment.Center)
      .row(Column())
      .row(Column(), Column(), Column(), Column(), Column(), Column(), Column(), Column(), Column(), Column())
      .row(Column(), Column())
      /**/ .content("AB", "KL")
      /**/ .content("CD", "MN")
      /**/ .content(List("EF", "GH", "IJ"), List("OP", "QR", "ST"))

    grid.size shouldBe 6
    grid(0).size shouldBe 1
    grid(0)(0).content shouldBe "A"
    grid(0)(0).widthType shouldBe ColumnWidth.Auto
    grid(0)(0).handleOverflow shouldBe OverflowHandling.Wrap
    grid(0)(0).alignment shouldBe Alignment.Left

    grid(1).size shouldBe 3
    grid(1)(0).content shouldBe "A\nA"
    grid(1)(0).widthType shouldBe ColumnWidth.Auto
    grid(1)(0).handleOverflow shouldBe OverflowHandling.Wrap
    grid(1)(0).alignment shouldBe Alignment.Left

    grid(1)(1).content shouldBe "B\nB"
    grid(1)(1).widthType shouldBe ColumnWidth.Fixed(5)
    grid(1)(1).handleOverflow shouldBe OverflowHandling.Wrap
    grid(1)(1).alignment shouldBe Alignment.Left

    grid(1)(2).content shouldBe "C\nC"
    grid(1)(2).widthType shouldBe ColumnWidth.Percentage(0.2)
    grid(1)(2).handleOverflow shouldBe OverflowHandling.Truncate
    grid(1)(2).alignment shouldBe Alignment.Center

    grid(2).size shouldBe 1
    grid(2)(0).content shouldBe ""
    grid(2)(0).handleOverflow shouldBe OverflowHandling.Wrap
    grid(2)(0).alignment shouldBe Alignment.Center


    grid(3).size shouldBe 1
    grid(3)(0).content shouldBe ""

    grid(4).size shouldBe 10
    grid(4)(0).content shouldBe ""

    grid(5).size shouldBe 2
    grid(5)(0).content shouldBe "AB\nCD\nEF\nGH\nIJ"
    grid(5)(1).content shouldBe "KL\nMN\nOP\nQR\nST"
  }

  it should "have correct column widths" in {

    // ┌────┬───────┬─────┐
    // │ AB │ 12345 │ CDE │
    // ├────┴┬──────┼─────┤
    // │ ABC │ 1234 │ DEF │
    // └─────┴──────┴─────┘
    val g = Grid(DefaultFormatting)
      .row(Column(), Column(), Column())
      .content("AB", "12345", "CDE")
      .row(Column(), Column(), Column())
      .content("ABC", "1234", "DEF")

    g(0).columnWidths shouldBe Seq(2, 5, 3)
    g(1).columnWidths shouldBe Seq(3, 4, 3)

  }

  it should "throw when given improper dimension" in {
    assertThrows[IllegalArgumentException] {
      Grid(DefaultFormatting)
        .row()
        .content("A", "B")
    }

    assertThrows[IllegalArgumentException] {
      Grid(DefaultFormatting)
        .row(Column(), Column(), Column())
        .content("A", "B")
    }

    assertThrows[IllegalArgumentException] {
      Grid(DefaultFormatting)
        .row(Column(), Column(), Column())
        .content(List("A", "B"), List("C", "D"))
    }

    assertThrows[IllegalArgumentException] {
      Grid(DefaultFormatting)
        .row(Column(), Column(), Column())
        .content(List("A", "B"), List("C", "D"))
    }
  }

  it should "be initialized with a default row" in {
    val grid = Grid(DefaultFormatting).content("ABC")
    grid.size shouldBe 1
    grid(0).size shouldBe 1
    grid(0)(0).content shouldBe "ABC"
    grid(0)(0).widthType shouldBe ColumnWidth.Auto
    grid(0)(0).alignment shouldBe Alignment.Left
  }

  it should "throw when line doesn't fit in column with OverflowHandling.Except" in {
    assertThrows[IllegalStateException] {
      Grid(DefaultFormatting)
        .row(Column(handleOverflow = Except))
        .content("ABCDEFGHIJKLMNOPQRSTUVXYZ")
        .toString
    }
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
      .row(Column(), Column())
      .content("ABC", "DEF")
      .toString shouldBe
      """|┌─────┬────────────┐
         |│ ABC │ DEF        │
         |└─────┴────────────┘""".stripMargin.trim
  }

  it should "render correctly with two columns and fixed width and centered text" in {
    Grid(DefaultFormatting)
      .row(Column(ColumnWidth.Fixed(3)), Column(alignment = Alignment.Center))
      .content("ABC", "DEF")
      .toString shouldBe
      """|┌─────┬────────────┐
         |│ ABC │    DEF     │
         |└─────┴────────────┘""".stripMargin
  }

  it should "render correctly with multiple rows and columns" in {
    Grid(DefaultFormatting)
      .row(Column(), Column(), Column())
      .content("AB", "12345", "CDE")
      .row(Column(), Column(), Column())
      .content("ABC", "1234", "DEF")
      .toString shouldBe
      """|┌────┬───────┬─────┐
         |│ AB │ 12345 │ CDE │
         |├────┴┬──────┼─────┤
         |│ ABC │ 1234 │ DEF │
         |└─────┴──────┴─────┘""".stripMargin

    Grid(DefaultFormatting)
      .row(Column(), Column())
      .content("ABC", "DEF")
      .row(Column(), Column())
      .content("ABC", "DEF")
      .toString shouldBe
      """|┌─────┬────────────┐
         |│ ABC │ DEF        │
         |├─────┼────────────┤
         |│ ABC │ DEF        │
         |└─────┴────────────┘""".stripMargin

  }

  it should "render correctly with different indentation" in {
    Grid(DefaultFormatting.copy(lineWidth = 30))
      .indent(5)
      .row(Column(), Column())
      .content("ABC", "DEF")
      .row(Column(), Column())
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
      .row(Column(), Column())
      .content("ABC", "DEF")
      .row(Column(), Column())
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
      .row(Column(), Column())
      .content("\u001b[31mABC\u001b[0m", "\u001b[32mDEF\u001b[0m")
      .row(Column(), Column())
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
      .row(Column(), Column(), Column())
      .content("ABCDEFGHIJKLMNOPQRSTUVXYZ", "ABCDEFGHIJKLMN", "ABCDEF")
      .row(Column(), Column(), Column())
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
  }


  it should "render correctly with line wrapping and ansi colors" in {
    Grid(DefaultFormatting)
      .row(Column(), Column(), Column())
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
      .row(Column(handleOverflow = Truncate), Column(handleOverflow = Truncate), Column(handleOverflow = Truncate))
      .content("ABCDEFGHIJKLMNOPQRSTUVXYZ", "ABCDEFGHIJKLMN", "AB")
      .row(Column(handleOverflow = Truncate), Column(handleOverflow = Truncate), Column(handleOverflow = Truncate))
      .content("ABC", "ABCDEFGHIJKLMNOPQRS", "ABCD")
      .content("ABCDEFGHIJKLMNOP", "ABC", "ABCDEFGHIJKLMNOPQRSTUVXYZ")
      .toString.print shouldBe
      """|┌──────┬──────┬────┐
         |│ A... │ A... │ AB │
         |├─────┬┴────┬─┴────┤
         |│ ABC │ ... │ ABCD │
         |│ ... │ ABC │ A... │
         |└─────┴─────┴──────┘""".stripMargin

  }

  it should "render correctly with truncation and ansi colors" in {
    Grid(DefaultFormatting)
      .row(Column(handleOverflow = Truncate), Column(handleOverflow = Truncate), Column(handleOverflow = Truncate))
      .content("\u001b[31mABCDEFGHIJKLMNOPQRSTUVXYZ\u001b[0m", "ABCDEFGHIJKLMN", "\u001b[32mAB\u001b[0m")
      .row(Column(handleOverflow = Truncate), Column(handleOverflow = Truncate), Column(handleOverflow = Truncate))
      .content("ABC", "\u001b[33mABCDEFGHIJKLMNOPQRS\u001b[0m", "\u001b[34mABCD\u001b[0m")
      .content("ABCDEFGHIJKLMNOP", "ABC", "\u001b[35mABC\u001b[36mDEF\u001b[37mGHIJKLMNOPQRSTUVXYZ\u001b[0m")
      .toString.print shouldBe
      s"""|┌──────┬──────┬────┐
          |│ \u001b[31mA\u001b[0m... │ A... │ \u001b[32mAB\u001b[0m │
          |├─────┬┴────┬─┴────┤
          |│ ABC │ ... │ \u001b[34mABCD\u001b[0m │
          |│ ... │ ABC │ \u001b[35mA\u001b[0m... │
          |└─────┴─────┴──────┘""".stripMargin
  }

  behavior of "Alignment"

  it should "left align correctly" in {
    Alignment.Left("", 10) shouldBe "          "
    Alignment.Left("ABC", 10) shouldBe "ABC       "
    Alignment.Left("ABCDEFGHIJ", 10) shouldBe "ABCDEFGHIJ"

    Alignment.Left("", 5) shouldBe "     "
    Alignment.Left("ABC", 5) shouldBe "ABC  "

    Alignment.Left("ABC", 10, '-') shouldBe "ABC-------"
  }

  it should "center correctly" in {
    Alignment.Center("", 10) shouldBe "          "
    Alignment.Center("AB", 10) shouldBe "    AB    "
    Alignment.Center("ABC", 10) shouldBe "   ABC    "
    Alignment.Center("ABCDEFGHIJ", 10) shouldBe "ABCDEFGHIJ"

    Alignment.Center("", 5) shouldBe "     "
    Alignment.Center("AB", 5) shouldBe " AB  "
    Alignment.Center("ABC", 5) shouldBe " ABC "
    Alignment.Center("ABCD", 5) shouldBe "ABCD "

    Alignment.Center("ABC", 10, '-') shouldBe "---ABC----"
  }

  it should "right align correctly" in {
    Alignment.Right("", 10) shouldBe "          "
    Alignment.Right("ABC", 10) shouldBe "       ABC"
    Alignment.Right("ABCDEFGHIJ", 10) shouldBe "ABCDEFGHIJ"

    Alignment.Right("", 5) shouldBe "     "
    Alignment.Right("ABC", 5) shouldBe "  ABC"

    Alignment.Right("ABC", 10, '-') shouldBe "-------ABC"
  }

  it should "align correctly with ansi colors" in {
    val str = "A\u001b[31mB\u001b[0mC"
    Alignment.Left(str, 10) shouldBe "A\u001b[31mB\u001b[0mC       "
    Alignment.Center(str, 10) shouldBe "   A\u001b[31mB\u001b[0mC    "
    Alignment.Right(str, 10) shouldBe "       A\u001b[31mB\u001b[0mC"
  }

  it should "throw when size is larger then width" in {
    assertThrows[IllegalArgumentException] { Alignment.Left("ABC", 2) }
    assertThrows[IllegalArgumentException] { Alignment.Center("ABC", 2) }
    assertThrows[IllegalArgumentException] { Alignment.Right("ABC", 2) }
  }

  behavior of "Truncation"

  it should "truncate regular text" in {
    val text = "ABCDEFGHIJKLMNO" // 15 chars long
    OverflowHandling.Truncate(text, 1).head shouldBe "."
    OverflowHandling.Truncate(text, 2).head shouldBe ".."
    OverflowHandling.Truncate(text, 3).head shouldBe "..."
    OverflowHandling.Truncate(text, 4).head shouldBe "A..."
    OverflowHandling.Truncate(text, 5).head shouldBe "AB..."
    OverflowHandling.Truncate(text, 10).head shouldBe "ABCDEFG..."
    OverflowHandling.Truncate(text, 14).head shouldBe "ABCDEFGHIJK..."
    OverflowHandling.Truncate(text, 15).head shouldBe "ABCDEFGHIJKLMNO"
    OverflowHandling.Truncate(text, 16).head shouldBe "ABCDEFGHIJKLMNO"
  }

  it should "truncate ansi colored text" in {
    val text = "\u001b[32mABC\u001b[33mDEF\u001b[34mGHI\u001b[35mJKL\u001b[36mMNO\u001b[0m" // 15 real chars long
    OverflowHandling.Truncate(text, 1).head shouldBe "."
    OverflowHandling.Truncate(text, 2).head shouldBe ".."
    OverflowHandling.Truncate(text, 3).head shouldBe "..."
    OverflowHandling.Truncate(text, 4).head shouldBe "\u001b[32mA\u001b[0m..."
    OverflowHandling.Truncate(text, 5).head shouldBe "\u001b[32mAB\u001b[0m..."
    OverflowHandling.Truncate(text, 10).head shouldBe "\u001b[32mABC\u001b[33mDEF\u001b[34mG\u001b[0m..."
    OverflowHandling.Truncate(text, 14).head shouldBe "\u001b[32mABC\u001b[33mDEF\u001b[34mGHI\u001b[35mJK\u001b[0m..."
    OverflowHandling.Truncate(text, 15).head shouldBe "\u001b[32mABC\u001b[33mDEF\u001b[34mGHI\u001b[35mJKL\u001b[36mMNO\u001b[0m"
    OverflowHandling.Truncate(text, 15).head should be theSameInstanceAs text
    OverflowHandling.Truncate(text, 16).head shouldBe "\u001b[32mABC\u001b[33mDEF\u001b[34mGHI\u001b[35mJKL\u001b[36mMNO\u001b[0m"
    OverflowHandling.Truncate(text, 16).head should be theSameInstanceAs text
  }

}
