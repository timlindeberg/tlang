package tlang.formatting

import tlang.formatting.Colors.{Color, ColorScheme}
import tlang.formatting.Colors.ColorScheme.DefaultColorScheme
import tlang.formatting.textformatters.{Coloring, Marking, SyntaxHighlighter}
import tlang.testutils.UnitSpec
import tlang.utils.Position

class SyntaxHighlighterSpec extends UnitSpec {


  behavior of "A syntax highlighter"


  it should "highlight valid code" in {
    val code =
      """|val a = 5
         |val b = "ABC"
         |// Comment
         |val c = 'D'
         |/* multi
         |line
         |comment */
         |val d = 1.0
         |""".stripMargin


    def colors(formatting: Formatting) = {
      import formatting._
      createColorings(
        (KeywordColor, 1, 1, 1, 4),
        (VarColor, 1, 5, 1, 6),
        (SymbolColor, 1, 7, 1, 8),
        (NumColor, 1, 9, 1, 10),
        (NoColor, 1, 10, 2, 1),
        (KeywordColor, 2, 1, 2, 4),
        (VarColor, 2, 5, 2, 6),
        (SymbolColor, 2, 7, 2, 8),
        (StringColor, 2, 9, 2, 14),
        (NoColor, 2, 14, 3, 1),
        (CommentColor, 3, 1, 3, 11),
        (NoColor, 3, 11, 4, 1),
        (KeywordColor, 4, 1, 4, 4),
        (VarColor, 4, 5, 4, 6),
        (SymbolColor, 4, 7, 4, 8),
        (StringColor, 4, 9, 4, 12),
        (NoColor, 4, 12, 5, 1),
        (CommentColor, 5, 1, 7, 11),
        (NoColor, 7, 11, 8, 1),
        (KeywordColor, 8, 1, 8, 4),
        (VarColor, 8, 5, 8, 6),
        (SymbolColor, 8, 7, 8, 8),
        (NumColor, 8, 9, 8, 12),
        (NoColor, 8, 12, 9, 1),
        (NoColor, 9, 1, 9, 1)
      )
    }


    test("With default color scheme") {
      val syntaxHighlighter = makeSyntaxHighlighter(colors(DefaultFormatting), DefaultFormatting)

      syntaxHighlighter(code) should matchWithAnsi(
        s"""|\u001b[34mval \u001b[36ma \u001b[37m= \u001b[35m5
            |\u001b[34mval \u001b[36mb \u001b[37m= \u001b[33m"ABC"
            |\u001b[30m// Comment
            |\u001b[34mval \u001b[36mc \u001b[37m= \u001b[33m'D'
            |\u001b[30m/* multi
            |line
            |comment */
            |\u001b[34mval \u001b[36md \u001b[37m= \u001b[35m1.0
            |\u001b[0m""".stripMargin
      )
    }

    test("With custom color scheme") {
      import Colors._

      val formatting = testFormatter(colorScheme = new ColorScheme {
        override def Comment: Int = GREEN
        override def Variable: Int = RED
        override def Keyword: Int = MAGENTA
        override def Symbol: Int = YELLOW
        override def Method: Int = NO_COLOR
        override def String: Int = CYAN
        override def Number: Int = BLUE
        override def Class: Int = NO_COLOR
      }).formatting

      val syntaxHighlighter = makeSyntaxHighlighter(colors(formatting), formatting)

      syntaxHighlighter(code) should matchWithAnsi(
        s"""|\u001b[35mval \u001b[31ma \u001b[33m= \u001b[34m5
            |\u001b[35mval \u001b[31mb \u001b[33m= \u001b[36m"ABC"
            |\u001b[32m// Comment
            |\u001b[35mval \u001b[31mc \u001b[33m= \u001b[36m'D'
            |\u001b[32m/* multi
            |line
            |comment */
            |\u001b[35mval \u001b[31md \u001b[33m= \u001b[34m1.0
            |\u001b[0m""".stripMargin
      )
    }

  }


  it should "not highlight code when colors are disabled" in {
    val formatting = Formatting(useColor = false)
    val colorings = mock[List[Coloring]]

    val syntaxHighlighter = SyntaxHighlighter(formatting)(_ => colorings)
    there were zeroInteractions(colorings)

    syntaxHighlighter("ABC DEF") shouldBe theSameInstanceAs("ABC DEF")
  }


  it should "not highlight NoColor tokens" in {
    val code =
      """|val a = 0b123456
         |val x = "ABC"
         |""".stripMargin

    val formatting = DefaultFormatting
    import formatting._


    val colorings = createColorings(
      (KeywordColor, 1, 1, 1, 4),
      (VarColor, 1, 5, 1, 6),
      (SymbolColor, 1, 7, 1, 8),
      (NoColor, 1, 9, 1, 17),
      (KeywordColor, 2, 1, 2, 4),
      (VarColor, 2, 5, 2, 6),
      (SymbolColor, 2, 7, 2, 8),
      (StringColor, 2, 9, 2, 14),
      (NoColor, 2, 14, 3, 1),
      (NoColor, 3, 1, 3, 1)
    )

    val syntaxHighlighter = makeSyntaxHighlighter(colorings, formatting)

    syntaxHighlighter(code) should matchWithAnsi(
      s"""|\u001b[34mval \u001b[36ma \u001b[37m= \u001b[0m0b123456
          |\u001b[34mval \u001b[36mx \u001b[37m= \u001b[33m"ABC"
          |\u001b[0m""".stripMargin
    )
  }


  it should "keep colors that are already in the given string" in {
    val code =
      s"""|val \u001b[31ma = 5
          |val \u001b[0mb = "ABC"
          |// Comment
          |val c = 'D'
          |/* \u001b[1;33mmul\u001b[0mti
          |line
          |commen\u001b[1;4;42mt */
          |""".stripMargin


    val formatting = DefaultFormatting
    import formatting._

    val colorings = createColorings(
      (KeywordColor, 1, 1, 1, 4),
      (VarColor, 1, 5, 1, 6),
      (SymbolColor, 1, 7, 1, 8),
      (NumColor, 1, 9, 1, 10),
      (NoColor, 1, 10, 2, 1),
      (KeywordColor, 2, 1, 2, 4),
      (VarColor, 2, 5, 2, 6),
      (SymbolColor, 2, 7, 2, 8),
      (StringColor, 2, 9, 2, 14),
      (NoColor, 2, 14, 3, 1),
      (CommentColor, 3, 1, 3, 11),
      (NoColor, 3, 11, 4, 1),
      (KeywordColor, 4, 1, 4, 4),
      (VarColor, 4, 5, 4, 6),
      (SymbolColor, 4, 7, 4, 8),
      (StringColor, 4, 9, 4, 12),
      (NoColor, 4, 12, 5, 1),
      (CommentColor, 5, 1, 7, 11),
      (NoColor, 7, 11, 8, 1),
      (NoColor, 8, 1, 8, 1)
    )

    val syntaxHighlighter = makeSyntaxHighlighter(colorings, formatting)

    syntaxHighlighter(code) should matchWithAnsi(
      s"""|\u001b[34mval \u001b[31ma = 5
          |val \u001b[36mb \u001b[37m= \u001b[33m"ABC"
          |\u001b[30m// Comment
          |\u001b[34mval \u001b[36mc \u001b[37m= \u001b[33m'D'
          |\u001b[30m/* \u001b[1;33mmul\u001b[0m\u001b[30mti
          |line
          |commen\u001b[0m\u001b[1;4;42mt */
          |\u001b[0m""".stripMargin
    )
  }


  it should "highlight markings" in {
    val code =
      s"""|val a = 5
          |val b = "ABC"
          |// Comment
          |val c = 'D'
          |/* multi
          |line
          |comment */
          |""".stripMargin

    val formatting = DefaultFormatting
    import formatting._

    val colorings = createColorings(
      (KeywordColor, 1, 1, 1, 4),
      (VarColor, 1, 5, 1, 6),
      (SymbolColor, 1, 7, 1, 8),
      (NumColor, 1, 9, 1, 10),
      (NoColor, 1, 10, 2, 1),
      (KeywordColor, 2, 1, 2, 4),
      (VarColor, 2, 5, 2, 6),
      (SymbolColor, 2, 7, 2, 8),
      (StringColor, 2, 9, 2, 14),
      (NoColor, 2, 14, 3, 1),
      (CommentColor, 3, 1, 3, 11),
      (NoColor, 3, 11, 4, 1),
      (KeywordColor, 4, 1, 4, 4),
      (VarColor, 4, 5, 4, 6),
      (SymbolColor, 4, 7, 4, 8),
      (StringColor, 4, 9, 4, 12),
      (NoColor, 4, 12, 5, 1),
      (CommentColor, 5, 1, 7, 11),
      (NoColor, 7, 11, 8, 1),
      (NoColor, 8, 1, 8, 1)
    )

    val markings = Seq(
      Marking(Position(1, 5, 2, 5), Red),
      Marking(Position(5, 4, 5, 7), Bold + Yellow),
      Marking(Position(7, 7, 7, 11), Bold + Underline + GreenBG)
    )

    val syntaxHighlighter = makeSyntaxHighlighter(colorings, formatting)
    syntaxHighlighter(code, markings) should matchWithAnsi(
      s"""|\u001b[34mval \u001b[31ma = 5
          |val \u001b[36mb \u001b[37m= \u001b[33m"ABC"
          |\u001b[30m// Comment
          |\u001b[34mval \u001b[36mc \u001b[37m= \u001b[33m'D'
          |\u001b[30m/* \u001b[1;33mmul\u001b[0m\u001b[30mti
          |line
          |commen\u001b[0m\u001b[1;4;42mt */
          |\u001b[0m""".stripMargin
    )
  }

  private def makeSyntaxHighlighter(colorings: List[Coloring], formatting: Formatting): SyntaxHighlighter = {
    SyntaxHighlighter(formatting)(_ => colorings)
  }

  private def createColorings(values: (Color, Int, Int, Int, Int)*): List[Coloring] = {
    values.map { case (color, line, col, endLine, endCol) =>
      Coloring(color, Position(line, col, endLine, endCol))}
      .toList
  }

}
