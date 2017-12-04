package tlang.formatting.grid

import tlang.formatting.grid.Alignment.{Center, Left, Right}
import tlang.formatting.grid.OverflowHandling.{Except, Truncate, Wrap}
import tlang.formatting.grid.Width.{Auto, Fixed, Percentage}
import tlang.formatting.textformatters.{Truncator, WordWrapper}
import tlang.formatting.{Colors, Formatter}
import tlang.testutils.UnitSpec
import tlang.utils.Extensions._


class GridSpec extends UnitSpec {

  val DefaultMaxWidth = 20


  behavior of "A Grid"

  it should "have correct grid size and attributes" in {
    val threeColumns = List(Column(width = Fixed(5)), Column, Column(overflowHandling = Except))
    val grid = Grid(mockedFormatter())
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
    val grid = Grid(mockedFormatter()).content("ABC")
    grid should have size 1
    grid(0) should have size 1
    grid(0)(0).content shouldBe "ABC"
    grid(0)(0).width shouldBe Width.Auto
    grid(0)(0).alignment shouldBe Alignment.Left
  }


  it should "have correct content" in {

    val sixTuple = ("A", "B", "C", "D", "E", "F")
    val grid = Grid(mockedFormatter(width = 10000))
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
    val grid = Grid(mockedFormatter(width = 10000))
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
    grid.render() shouldBe ""

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
    var grid = Grid(mockedFormatter())
      .row()
      .content("A")

    grid(0).columnWidths should contain theSameElementsInOrderAs Seq(16)

    // ┌──────────────────┐
    // │ A                │
    // └──────────────────┘
    grid = Grid(mockedFormatter())
      .row(Column(Width.Fixed(5)))
      .content("A")

    grid(0).columnWidths should contain theSameElementsInOrderAs Seq(16)

    // ┌───┬──────────────┐
    // │ A │ B            │
    // └───┴──────────────┘
    grid = Grid(mockedFormatter())
      .row(Column, Column)
      .content("A", "B")


    grid(0).columnWidths should contain theSameElementsInOrderAs Seq(1, 12)

    // ┌────┬─────────────┐
    // │ AB │ C           │
    // └────┴─────────────┘
    grid = Grid(mockedFormatter())
      .row(Column, Column)
      .content("AB", "C")

    grid(0).columnWidths should contain theSameElementsInOrderAs Seq(2, 11)

    // ┌───┬──────────────┐
    // │ A │ BC           │
    // └───┴──────────────┘
    grid = Grid(mockedFormatter())
      .row(Column, Column)
      .content("A", "BC")

    grid(0).columnWidths should contain theSameElementsInOrderAs Seq(1, 12)

    // ┌───┬───┬───┬──────┐
    // │ A │ B │ C │ D    │
    // └───┴───┴───┴──────┘
    grid = Grid(mockedFormatter())
      .row(Column, Column, Column, Column)
      .content("A", "B", "C", "D")

    grid(0).columnWidths should contain theSameElementsInOrderAs Seq(1, 1, 1, 4)

    // ┌────────┬─────────┐
    // │ ABC    │ DEF     │
    // └────────┴─────────┘
    grid = Grid(mockedFormatter())
      .row(Column(Width.Percentage(0.5)), Column)
      .content("ABC", "DEF")

    grid(0).columnWidths should contain theSameElementsInOrderAs Seq(6, 7)

    // ┌──────────┬───────┐
    // │ ABC      │ D     │
    // └──────────┴───────┘
    grid = Grid(mockedFormatter())
      .row(Column, Column(Width.Fixed(5)))
      .content("ABC", "D")

    grid(0).columnWidths should contain theSameElementsInOrderAs Seq(8, 5)


    // ┌────┬───────┬─────┐
    // │ AB │ D     │ DE  │
    // └────┴───────┴─────┘
    grid = Grid(mockedFormatter())
      .row(Column, Column(Width.Fixed(5)), Column)
      .content("AB", "C", "DE")

    grid(0).columnWidths should contain theSameElementsInOrderAs Seq(2, 5, 3)

    // ┌─────┬─────┬──────┐
    // │ A   │ B   │ C    │
    // └─────┴─────┴──────┘
    grid = Grid(mockedFormatter())
      .row(Column(Width.Percentage(0.33)), Column(Width.Percentage(0.33)), Column(Width.Percentage(0.33)))
      .content("A", "B", "C")

    grid(0).columnWidths should contain theSameElementsInOrderAs Seq(3, 3, 4)

    // ┌──────┬──────┬────┐
    // │ A    │ B    │ C  │
    // └──────┴──────┴────┘
    grid = Grid(mockedFormatter())
      .row(Column(Width.Fixed(4)), Column(Width.Fixed(4)), Column)
      .content("A", "B", "C")

    grid(0).columnWidths should contain theSameElementsInOrderAs Seq(4, 4, 2)

    // ┌───────┬─────┬────┐
    // │ A     │ BCD │ E  │
    // └───────┴─────┴────┘
    grid = Grid(mockedFormatter())
      .row(Column(Width.Fixed(5)), Column, Column)
      .content("A", "BCD", "E")

    grid(0).columnWidths should contain theSameElementsInOrderAs Seq(5, 3, 2)

    // ┌────────┬─────────┐
    // │ ABCDEF │ ABCDEFG │
    // │ GHIJKL │ HIJKLMN │
    // │ MNO    │ O       │
    // └────────┴─────────┘
    grid = Grid(mockedFormatter())
      .row(Column, Column)
      .content("ABCDEFGHIJKLMNO", "ABCDEFGHIJKLMNO")

    grid(0).columnWidths should contain theSameElementsInOrderAs Seq(6, 7)

    // ┌────┬───────┬─────┐
    // │ AB │ 12345 │ CDE │
    // ├────┴┬──────┼─────┤
    // │ ABC │ 1234 │ DEF │
    // └─────┴──────┴─────┘
    grid = Grid(mockedFormatter())
      .row(Column, Column, Column)
      .content("AB", "12345", "CDE")
      .row(Column, Column, Column)
      .content("ABC", "1234", "DEF")

    grid(0).columnWidths should contain theSameElementsInOrderAs Seq(2, 5, 3)
    grid(1).columnWidths should contain theSameElementsInOrderAs Seq(3, 4, 3)
  }


  it should "should be able to accept the same column multiple times" in {
    var grid = Grid(mockedFormatter())
      .row(Column, Column)
      .content("AB", "12345")

    grid(0)(0).content shouldBe "AB"
    grid(0)(1).content shouldBe "12345"

    grid = Grid(mockedFormatter())
      .row(CenteredColumn, CenteredColumn)
      .content("AB", "12345")

    grid(0)(0).content shouldBe "AB"
    grid(0)(1).content shouldBe "12345"
    grid(0)(0).alignment shouldBe Center
    grid(0)(1).alignment shouldBe Center

    val RightAlignedTruncatedColumn = Column(alignment = Right, overflowHandling = Truncate)

    grid = Grid(mockedFormatter())
      .row(RightAlignedTruncatedColumn, RightAlignedTruncatedColumn)
      .content("AB", "12345")

    grid(0)(0).content shouldBe "AB"
    grid(0)(1).content shouldBe "12345"
    grid(0)(0).alignment shouldBe Right
    grid(0)(1).alignment shouldBe Right
    grid(0)(0).overflowHandling shouldBe Truncate
    grid(0)(1).overflowHandling shouldBe Truncate
  }


  it should "render correctly with a single row" in {
    Grid(mockedFormatter())
      .row()
      .content("ABC")
      .render() shouldBe
      """|┌──────────────────┐
         |│ ABC              │
         |└──────────────────┘""".stripMargin
  }


  it should "render correctly with a centered text" in {
    Grid(mockedFormatter())
      .row(Column(alignment = Center))
      .content("ABC")
      .render() shouldBe
      """|┌──────────────────┐
         |│       ABC        │
         |└──────────────────┘""".stripMargin
  }


  it should "render correctly with a right aligned text" in {
    Grid(mockedFormatter())
      .row(Column(alignment = Right))
      .content("ABC")
      .render() shouldBe
      """|┌──────────────────┐
         |│              ABC │
         |└──────────────────┘""".stripMargin
  }


  it should "render correctly with two columns" in {
    Grid(mockedFormatter())
      .row(Column, Column)
      .content("ABC", "DEF")
      .render() shouldBe
      """|┌─────┬────────────┐
         |│ ABC │ DEF        │
         |└─────┴────────────┘""".stripMargin
  }


  it should "render correctly with two columns and fixed width and centered text" in {
    Grid(mockedFormatter())
      .row(Column(Width.Fixed(3)), Column(alignment = Alignment.Center))
      .content("ABC", "DEF")
      .render() shouldBe
      """|┌─────┬────────────┐
         |│ ABC │    DEF     │
         |└─────┴────────────┘""".stripMargin
  }


  it should "render correctly with multiple rows and columns" in {
    Grid(mockedFormatter())
      .row(Column, Column, Column)
      .content("AB", "12345", "CDE")
      .row(Column, Column, Column)
      .content("ABC", "1234", "DEF")
      .render() shouldBe
      """|┌────┬───────┬─────┐
         |│ AB │ 12345 │ CDE │
         |├────┴┬──────┼─────┤
         |│ ABC │ 1234 │ DEF │
         |└─────┴──────┴─────┘""".stripMargin

    Grid(mockedFormatter())
      .row(Column, Column)
      .content("ABC", "DEF")
      .row(Column, Column)
      .content("ABC", "DEF")
      .render() shouldBe
      """|┌─────┬────────────┐
         |│ ABC │ DEF        │
         |├─────┼────────────┤
         |│ ABC │ DEF        │
         |└─────┴────────────┘""".stripMargin
  }


  it should "render correctly with fixed and auto column widths" in {
    val wordWrapper = mock[WordWrapper]
    wordWrapper.wrapAnsiFormatting(*) forwardsArg 0
    wordWrapper.apply("ABCDEFG", 4) returns List("ABCD", "EFG")
    wordWrapper.apply("12345", 2) returns List("12", "34", "5")
    wordWrapper.apply("HIJKMLN", 4) returns List("HIJK", "LMN")

    wordWrapper.apply("ABC", 3) returns List("ABC")
    wordWrapper.apply("12345678", 5) returns List("12345", "678")
    wordWrapper.apply("DEFGH", 2) returns List("DE", "FG", "H")

    Grid(mockedFormatter(wordWrapper = wordWrapper))
      .row(Column, Column(width = Fixed(2)), Column)
      .content("ABCDEFG", "12345", "HIJKMLN")
      .row(Column, Column, Column(width = Fixed(2)))
      .content("ABC", "12345678", "DEFGH")
      .render() shouldBe
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
    Grid(mockedFormatter(width = 30))
      .indent(5)
      .row(Column, Column)
      .content("ABC", "DEF")
      .row(Column, Column)
      .content("ABC", "DEF")
      .render() shouldBe
      """|┌─────────────┬──────────────┐
         |│     ABC     │     DEF      │
         |├─────────────┼──────────────┤
         |│     ABC     │     DEF      │
         |└─────────────┴──────────────┘""".stripMargin
  }


  it should "render correctly with a header" in {
    Grid(mockedFormatter())
      .header("Header")
      .row(Column, Column, Column)
      .content("ABC", "DEFG", "HIJ")
      .render() shouldBe
      """|╒══════════════════╕
         |│      Header      │
         |╞═════╤══════╤═════╡
         |│ ABC │ DEFG │ HIJ │
         |└─────┴──────┴─────┘""".stripMargin
  }


  it should "render correctly with and without unicode characters" in {
    val grid = Grid(mockedFormatter())
      .header("Header")
      .row(Column, Column)
      .content("ABC", "DEF")
      .row(Column, Column)
      .content("ABC", "DEF")

    grid.render() shouldBe
      """|╒══════════════════╕
         |│      Header      │
         |╞═════╤════════════╡
         |│ ABC │ DEF        │
         |├─────┼────────────┤
         |│ ABC │ DEF        │
         |└─────┴────────────┘""".stripMargin

    grid.formatter(mockedFormatter(asciiOnly = true)).render() shouldBe
      " ================== " + NL + // So intellij wont trim the whitespace at the end of the line
        """||      Header      |
           ||==================|
           || ABC | DEF        |
           ||-----+------------|
           || ABC | DEF        |
           | ------------------ """.stripMargin
  }


  it should "render correctly with ansi colors" in {
    Grid(mockedFormatter())
      .row(Column, Column)
      .content("\u001b[31mABC\u001b[0m", "\u001b[32mDEF\u001b[0m")
      .row(Column, Column)
      .content("A\u001b[1mB\u001b[0mC", "\u001b[4mD\u001b[0mEF")
      .render() should matchWithAnsi(
      s"""|┌─────┬────────────┐
          |│ \u001b[31mABC\u001b[0m │ \u001b[32mDEF\u001b[0m        │
          |├─────┼────────────┤
          |│ A\u001b[1mB\u001b[0mC │ \u001b[4mD\u001b[0mEF        │
          |└─────┴────────────┘""".stripMargin
    )
    val wordWrapper = mock[WordWrapper]
    wordWrapper.wrapAnsiFormatting(*) forwardsArg 0

    wordWrapper.apply("\u001b[31mABCDEFGHIJKLMNOPQRSTUVXYZ\u001b[0m", 16) returns List(
      "\u001b[31mABCDEFGHIJKLMNOP\u001b[0m",
      "\u001b[31mQRSTUVXYZ\u001b[0m"
    )
    Grid(mockedFormatter(wordWrapper = wordWrapper))
      .row()
      .content("\u001b[31mABCDEFGHIJKLMNOPQRSTUVXYZ\u001b[0m")
      .render() should matchWithAnsi(
      s"""|┌──────────────────┐
          |│ \u001b[31mABCDEFGHIJKLMNOP\u001b[0m │
          |│ \u001b[31mQRSTUVXYZ\u001b[0m        │
          |└──────────────────┘""".stripMargin)
  }


  it should "render correctly with colored borders" in {
    Grid(mockedFormatter())
      .borderColor(Colors.Red)
      .row(Column, Column)
      .content("ABC", "DEF")
      .row(Column, Column)
      .content("ABC", "DEF")
      .render() should matchWithAnsi(
      """|\u001b[31m┌─────┬────────────┐\u001b[0m
         |\u001b[31m│\u001b[0m ABC \u001b[31m│\u001b[0m DEF        \u001b[31m│\u001b[0m
         |\u001b[31m├─────┼────────────┤\u001b[0m
         |\u001b[31m│\u001b[0m ABC \u001b[31m│\u001b[0m DEF        \u001b[31m│\u001b[0m
         |\u001b[31m└─────┴────────────┘\u001b[0m""".stripMargin)
  }


  it should "render correctly with line wrapping" in {
    var wordWrapper = mock[WordWrapper]
    wordWrapper.wrapAnsiFormatting(*) forwardsArg 0


    wordWrapper.apply("ABCDEFGHIJKLMNOPQRSTUVXYZ", 4) returns List("ABCD", "EFGH", "IJKL", "MNOP", "QRST", "UVXY", "Z")
    wordWrapper.apply("ABCDEFGHIJKLMN", 3) returns List("ABC", "DEF", "GHI", "JKL", "MN")
    wordWrapper.apply("ABCDEF", 3) returns List("ABC", "DEF")

    wordWrapper.apply("ABCDEFGHIJ", 3) returns List("ABC", "DEF", "GHI", "J")
    wordWrapper.apply("ABCDEFGHIJKLMNOPQRS", 3) returns List("ABC", "DEF", "GHI", "JKL", "MNO", "PQR", "S")
    wordWrapper.apply("ABCD", 4) returns List("ABCD")

    wordWrapper.apply("ABCDEFGHIJKLMNOP", 3) returns List("ABC", "DEF", "GHI", "JKL", "MNO", "P")
    wordWrapper.apply("ABCDEFG", 3) returns List("ABC", "DEF", "G")
    wordWrapper.apply("ABCDEFGHIJKLMNOPQRSTUVXYZ", 4) returns List("ABCD", "EFGH", "IJKL", "MNOP", "QRST", "UVXY", "Z")

    Grid(mockedFormatter(wordWrapper = wordWrapper))
      .row(Column, Column, Column)
      .content("ABCDEFGHIJKLMNOPQRSTUVXYZ", "ABCDEFGHIJKLMN", "ABCDEF")
      .row(Column, Column, Column)
      .content("ABCDEFGHIJ", "ABCDEFGHIJKLMNOPQRS", "ABCD")
      .content("ABCDEFGHIJKLMNOP", "ABCDEFG", "ABCDEFGHIJKLMNOPQRSTUVXYZ")
      .render() shouldBe
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

    wordWrapper = mock[WordWrapper]
    wordWrapper.apply("ABCDEFGHIJKL", 1) returns List("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L")
    wordWrapper.apply("ABCDEFGHI", 1) returns List("A", "B", "C", "D", "E", "F", "G", "H", "I")
    wordWrapper.apply("ABCDEF", 1) returns List("A", "B", "C", "D", "E", "F")
    wordWrapper.apply("ABC", 1) returns List("A", "B", "C")
    wordWrapper.apply("A", 1) returns List("A")

    Grid(mockedFormatter(width = 21, wordWrapper = wordWrapper))
      .row(Column, Column, Column, Column, Column)
      .content("ABCDEFGHIJKL", "ABCDEFGHI", "ABCDEF", "ABC", "A")
      .render() shouldBe
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


  it should "render correctly with column headers" in {
    Grid(mockedFormatter())
      .row(Column, Column)
      .columnHeaders("ABC", "DEF")
      .content("ABC", "DEF")
      .render() should matchWithAnsi(
      s"""|┌─────┬────────────┐
          |│ \u001b[1;34mABC\u001b[0m │ \u001b[1;34mDEF\u001b[0m        │
          |│     │            │
          |│ ABC │ DEF        │
          |└─────┴────────────┘""".stripMargin.trim
    )

    val formatter = mockedFormatter()
    Grid(formatter)
      .columnHeaderColor(formatter.formatting.Red)
      .row(Column, Column)
      .columnHeaders("ABC", "DEF")
      .content("ABC", "DEF")
      .render() should matchWithAnsi(
      s"""|┌─────┬────────────┐
          |│ \u001b[31mABC\u001b[0m │ \u001b[31mDEF\u001b[0m        │
          |│     │            │
          |│ ABC │ DEF        │
          |└─────┴────────────┘""".stripMargin.trim
    )


  }


  it should "render correctly with empty columns" in {
    Grid(mockedFormatter())
      .row(Column, Column)
      .content("", "ABC")
      .render() shouldBe
      """|┌───┬──────────────┐
         |│   │ ABC          │
         |└───┴──────────────┘""".stripMargin.trim

    Grid(mockedFormatter())
      .row(Column, Column, Column, Column)
      .content("", "ABC", "", "")
      .row()
      .row(Column, Column)
      .content("", "")
      .row(Column, Column, Column, Column)
      .render() shouldBe
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
    val wordWrapper = mock[WordWrapper]

    wordWrapper.apply("\u001b[31mABCD\u001b[32mEFGH\u001b[33mIJKL\u001b[34mMNOP\u001b[35mQRST\u001b[36mUVXY\u001b[37mZ\u001b[0m", 4) returns List(
      "\u001b[31mABCD\u001b[0m",
      "\u001b[32mEFGH\u001b[0m",
      "\u001b[33mIJKL\u001b[0m",
      "\u001b[34mMNOP\u001b[0m",
      "\u001b[35mQRST\u001b[0m",
      "\u001b[36mUVXY\u001b[0m",
      "\u001b[37mZ\u001b[0m"
    )
    wordWrapper.apply("\u001b[31mABCD\u001b[32mEFGH\u001b[33mIJKL\u001b[34mMN\u001b[0m", 3) returns List(
      "\u001b[31mABC\u001b[0m",
      "\u001b[31mD\u001b[32mEF\u001b[0m",
      "\u001b[32mGH\u001b[33mI\u001b[0m",
      "\u001b[33mJKL\u001b[0m",
      "\u001b[34mMN\u001b[0m"
    )
    wordWrapper.apply("AB\u001b[31mCDE\u001b[0mF", 3) returns List(
      "AB\u001b[31mC\u001b[0m",
      "\u001b[31mDE\u001b[0mF"
    )
    wordWrapper.wrapAnsiFormatting(*) forwardsArg 0

    Grid(mockedFormatter(wordWrapper = wordWrapper))
      .row(Column, Column, Column)
      .content("\u001b[31mABCD\u001b[32mEFGH\u001b[33mIJKL\u001b[34mMNOP\u001b[35mQRST\u001b[36mUVXY\u001b[37mZ\u001b[0m",
        "\u001b[31mABCD\u001b[32mEFGH\u001b[33mIJKL\u001b[34mMN\u001b[0m",
        "AB\u001b[31mCDE\u001b[0mF"
      )
      .render() should matchWithAnsi(
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
    val truncator = mock[Truncator]
    truncator.apply("ABCDEFGHIJKLMNOPQRSTUVXYZ", 4) returns "A..."
    truncator.apply("ABCDEFGHIJKLMN", 4) returns "A..."
    truncator.apply("AB", 2) returns "AB"

    truncator.apply("ABC", 3) returns "ABC"
    truncator.apply("ABCDEFGHIJKLMNOPQRS", 3) returns "..."
    truncator.apply("ABCD", 4) returns "ABCD"

    truncator.apply("ABCDEFGHIJKLMNOP", 3) returns "..."
    truncator.apply("ABC", 3) returns "ABC"
    truncator.apply("ABCDEFGHIJKLMNOPQRSTUVXYZ", 4) returns "A..."

    Grid(mockedFormatter(truncator = truncator))
      .row(TruncatedColumn, TruncatedColumn, TruncatedColumn)
      .content("ABCDEFGHIJKLMNOPQRSTUVXYZ", "ABCDEFGHIJKLMN", "AB")
      .row(TruncatedColumn, TruncatedColumn, TruncatedColumn)
      .content("ABC", "ABCDEFGHIJKLMNOPQRS", "ABCD")
      .content("ABCDEFGHIJKLMNOP", "ABC", "ABCDEFGHIJKLMNOPQRSTUVXYZ")
      .render() shouldBe
      """|┌──────┬──────┬────┐
         |│ A... │ A... │ AB │
         |├─────┬┴────┬─┴────┤
         |│ ABC │ ... │ ABCD │
         |│ ... │ ABC │ A... │
         |└─────┴─────┴──────┘""".stripMargin
  }


  it should "render correctly with truncation and ansi colors" in {
    val truncator = mock[Truncator]
    truncator.apply("\u001b[31mABCDEFGHIJKLMNOPQRSTUVXYZ\u001b[0m", 4) returns "\u001b[31mA\u001b[0m..."
    truncator.apply("ABCDEFGHIJKLMN", 4) returns "A..."
    truncator.apply("\u001b[32mAB\u001b[0m", 2) returns "\u001b[32mAB\u001b[0m"

    truncator.apply("ABC", 3) returns "ABC"
    truncator.apply("\u001b[33mABCDEFGHIJKLMNOPQRS\u001b[0m", 3) returns "..."
    truncator.apply("\u001b[34mABCD\u001b[0m", 4) returns "\u001b[34mABCD\u001b[0m"

    truncator.apply("ABCDEFGHIJKLMNOP", 3) returns "..."
    truncator.apply("ABC", 3) returns "ABC"
    truncator.apply("\u001b[35mABC\u001b[36mDEF\u001b[37mGHIJKLMNOPQRSTUVXYZ\u001b[0m", 4) returns "\u001b[35mA\u001b[0m..."

    Grid(mockedFormatter(truncator = truncator))
      .row(TruncatedColumn, TruncatedColumn, TruncatedColumn)
      .content("\u001b[31mABCDEFGHIJKLMNOPQRSTUVXYZ\u001b[0m", "ABCDEFGHIJKLMN", "\u001b[32mAB\u001b[0m")
      .row(TruncatedColumn, TruncatedColumn, TruncatedColumn)
      .content("ABC", "\u001b[33mABCDEFGHIJKLMNOPQRS\u001b[0m", "\u001b[34mABCD\u001b[0m")
      .content("ABCDEFGHIJKLMNOP", "ABC", "\u001b[35mABC\u001b[36mDEF\u001b[37mGHIJKLMNOPQRSTUVXYZ\u001b[0m")
      .render() should matchWithAnsi(
      s"""|┌──────┬──────┬────┐
          |│ \u001b[31mA\u001b[0m... │ A... │ \u001b[32mAB\u001b[0m │
          |├─────┬┴────┬─┴────┤
          |│ ABC │ ... │ \u001b[34mABCD\u001b[0m │
          |│ ... │ ABC │ \u001b[35mA\u001b[0m... │
          |└─────┴─────┴──────┘""".stripMargin)
  }


  it should "render correctly with newlines in content" in {
    Grid(mockedFormatter())
      .row()
      .content(s"ABC${ NL }DEF${ NL }GHI")
      .render() shouldBe
      """|┌──────────────────┐
         |│ ABC              │
         |│ DEF              │
         |│ GHI              │
         |└──────────────────┘""".stripMargin

    Grid(mockedFormatter())
      .row()
      .content(s"${ NL }ABC${ NL }${ NL }DEF${ NL }${ NL }GHI${ NL }")
      .render() shouldBe
      """|┌──────────────────┐
         |│                  │
         |│ ABC              │
         |│                  │
         |│ DEF              │
         |│                  │
         |│ GHI              │
         |│                  │
         |└──────────────────┘""".stripMargin

    Grid(mockedFormatter())
      .header("ABC")
      .row()
      .content(s"1+1\n")
      .render() shouldBe
      """|╒══════════════════╕
         |│       ABC        │
         |╞══════════════════╡
         |│ 1+1              │
         |│                  │
         |└──────────────────┘""".stripMargin

    Grid(mockedFormatter())
      .row()
      .content(s"\u001b[31m1+1\u001b[0m${ NL }\u001b[31m")
      .render() should matchWithAnsi(
      s"""|┌──────────────────┐
          |│ \u001b[31m1+1\u001b[0m              │
          |│ \u001b[31m                 │
          |└──────────────────┘""".stripMargin
    )

  }


  it should "throw when given content with improper dimension" in {
    intercept[IllegalArgumentException] {
      Grid(mockedFormatter())
        .row()
        .content("A", "B")

    }.getMessage should include("2 != 1")

    intercept[IllegalArgumentException] {
      Grid(mockedFormatter())
        .row(Column, Column, Column)
        .content("A", "B")

    }.getMessage should include("2 != 3")

    intercept[IllegalArgumentException] {
      Grid(mockedFormatter())
        .row(Column, Column, Column)
        .contents(List(("A", "B"), ("C", "D")))

    }.getMessage should include("2 != 3")
  }


  it should "throw when line doesn't fit in column with OverflowHandling.Except" in {
    intercept[IllegalStateException] {
      Grid(mockedFormatter())
        .row(Column(overflowHandling = Except))
        .content("ABCDEFGHIJKLMNOPQRSTUVXYZ")
        .content("ABC")
        .render()

    }.getMessage should (include("25 > 16") and include("ABCDEFGHIJKLMNOPQRSTUVXYZ"))
  }


  it should "throw when columns cannot fit in the row" in {
    intercept[IllegalStateException] {
      Grid(mockedFormatter()).row(Column(width = Fixed(25)))

    }.getMessage should include("29 > 20")

    intercept[IllegalStateException] {
      Grid(mockedFormatter()).row(Column(width = Percentage(0.9)), Column, Column)

    }.getMessage should include("21 > 20")

    intercept[IllegalStateException] {
      Grid(mockedFormatter()).row(Column(width = Percentage(0.9)), Column(width = Fixed(3)))

    }.getMessage should include("21 > 20")

    intercept[IllegalStateException] {
      Grid(mockedFormatter()).row(Column, Column, Column, Column, Column)

    }.getMessage should include("21 > 20")

    intercept[IllegalStateException] {
      Grid(mockedFormatter()).row(Column(width = Fixed(5)), Column(width = Fixed(5)), Column)

    }.getMessage should include("21 > 20")
  }


  it should "throw when adding a header once a rows been added" in {
    intercept[IllegalStateException] {
      Grid(mockedFormatter()).row().header("Header")
    }

    intercept[IllegalStateException] {
      Grid(mockedFormatter())
        .content("ABC")
        .header("Header")
    }

    intercept[IllegalStateException] {
      Grid(mockedFormatter())
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
      Grid(mockedFormatter())
        .contents(List(Nil))

    }.getMessage should include("Nil")

    // Column is a case class and there for implements Product
    intercept[IllegalArgumentException] {
      Grid(mockedFormatter())
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


  private def mockedFormatter(
    width: Int = DefaultMaxWidth,
    asciiOnly: Boolean = false,
    wordWrapper: WordWrapper = mockedWordWrapperReturningSplitLines,
    truncator: Truncator = mock[Truncator]
  ): Formatter = {
    createMockFormatter(width = width, asciiOnly = asciiOnly, wordWrapper = wordWrapper, truncator = truncator)
  }


}
