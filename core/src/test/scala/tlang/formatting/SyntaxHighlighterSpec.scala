package tlang.formatting

import tlang.compiler.lexer.Tokens._
import tlang.compiler.lexer.{Lexer, Token, TokenKind}
import tlang.formatting.Colors.ColorScheme
import tlang.formatting.Colors.ColorScheme.DefaultColorScheme
import tlang.formatting.textformatters.{Marking, SyntaxHighlighter}
import tlang.messages.{ErrorStringContext, VoidReporter}
import tlang.testutils.UnitSpec
import tlang.utils.Extensions._
import tlang.utils.{Position, Source, StringSource}

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

    val lexer = mock[Lexer]
    lexer.apply(StringSource(code, "")) returns tokenList(
      (PRIVVAL, 1, 1, 1, 4),
      (IDKIND, 1, 5, 1, 6),
      (EQSIGN, 1, 7, 1, 8),
      (INTLITKIND, 1, 9, 1, 10),
      (NEWLINE, 1, 10, 2, 1),
      (PRIVVAL, 2, 1, 2, 4),
      (IDKIND, 2, 5, 2, 6),
      (EQSIGN, 2, 7, 2, 8),
      (STRLITKIND, 2, 9, 2, 14),
      (NEWLINE, 2, 14, 3, 1),
      (COMMENTLITKIND, 3, 1, 3, 11),
      (NEWLINE, 3, 11, 4, 1),
      (PRIVVAL, 4, 1, 4, 4),
      (IDKIND, 4, 5, 4, 6),
      (EQSIGN, 4, 7, 4, 8),
      (CHARLITKIND, 4, 9, 4, 12),
      (NEWLINE, 4, 12, 5, 1),
      (COMMENTLITKIND, 5, 1, 7, 11),
      (NEWLINE, 7, 11, 8, 1),
      (PRIVVAL, 8, 1, 8, 4),
      (IDKIND, 8, 5, 8, 6),
      (EQSIGN, 8, 7, 8, 8),
      (DOUBLELITKIND, 8, 9, 8, 12),
      (NEWLINE, 8, 12, 9, 1),
      (EOF, 9, 1, 9, 1)
    )

    test("With default color scheme") {
      val syntaxHighlighter = makeSyntaxHighlighter(lexer)

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

      val syntaxHighlighter = makeSyntaxHighlighter(lexer, colorScheme = new ColorScheme {
        override def Comment: Int = GREEN
        override def Variable: Int = RED
        override def Keyword: Int = MAGENTA
        override def Symbol: Int = YELLOW
        override def Method: Int = NO_COLOR
        override def String: Int = CYAN
        override def Number: Int = BLUE
        override def Class: Int = NO_COLOR
      })

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
    val lexer = mock[Lexer]
    val syntaxHighlighter = SyntaxHighlighter(lexer, formatting)
    there were zeroInteractions(lexer)

    syntaxHighlighter("ABC DEF") shouldBe theSameInstanceAs("ABC DEF")
  }


  it should "not highlight bad tokens" in {
    val code =
      """|val a = 0b123456
         |val x = "ABC"
         |""".stripMargin

    val lexer = mock[Lexer]
    lexer.apply(StringSource(code, "")) returns tokenList(
      (PRIVVAL, 1, 1, 1, 4),
      (IDKIND, 1, 5, 1, 6),
      (EQSIGN, 1, 7, 1, 8),
      (BAD, 1, 9, 1, 17),
      (PRIVVAL, 2, 1, 2, 4),
      (IDKIND, 2, 5, 2, 6),
      (EQSIGN, 2, 7, 2, 8),
      (STRLITKIND, 2, 9, 2, 14),
      (NEWLINE, 2, 14, 3, 1),
      (EOF, 3, 1, 3, 1)
    )

    val syntaxHighlighter = makeSyntaxHighlighter(lexer)

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

    val lexer = mock[Lexer]
    lexer.apply(StringSource(code.stripAnsi, "")) returns tokenList(
      (PRIVVAL, 1, 1, 1, 4),
      (IDKIND, 1, 5, 1, 6),
      (EQSIGN, 1, 7, 1, 8),
      (INTLITKIND, 1, 9, 1, 10),
      (NEWLINE, 1, 10, 2, 1),
      (PRIVVAL, 2, 1, 2, 4),
      (IDKIND, 2, 5, 2, 6),
      (EQSIGN, 2, 7, 2, 8),
      (STRLITKIND, 2, 9, 2, 14),
      (NEWLINE, 2, 14, 3, 1),
      (COMMENTLITKIND, 3, 1, 3, 11),
      (NEWLINE, 3, 11, 4, 1),
      (PRIVVAL, 4, 1, 4, 4),
      (IDKIND, 4, 5, 4, 6),
      (EQSIGN, 4, 7, 4, 8),
      (CHARLITKIND, 4, 9, 4, 12),
      (NEWLINE, 4, 12, 5, 1),
      (COMMENTLITKIND, 5, 1, 7, 11),
      (NEWLINE, 7, 11, 8, 1),
      (EOF, 8, 1, 8, 1)
    )

    val syntaxHighlighter = makeSyntaxHighlighter(lexer)

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

    val lexer = mock[Lexer]
    lexer.apply(StringSource(code.stripAnsi, "")) returns tokenList(
      (PRIVVAL, 1, 1, 1, 4),
      (IDKIND, 1, 5, 1, 6),
      (EQSIGN, 1, 7, 1, 8),
      (INTLITKIND, 1, 9, 1, 10),
      (NEWLINE, 1, 10, 2, 1),
      (PRIVVAL, 2, 1, 2, 4),
      (IDKIND, 2, 5, 2, 6),
      (EQSIGN, 2, 7, 2, 8),
      (STRLITKIND, 2, 9, 2, 14),
      (NEWLINE, 2, 14, 3, 1),
      (COMMENTLITKIND, 3, 1, 3, 11),
      (NEWLINE, 3, 11, 4, 1),
      (PRIVVAL, 4, 1, 4, 4),
      (IDKIND, 4, 5, 4, 6),
      (EQSIGN, 4, 7, 4, 8),
      (CHARLITKIND, 4, 9, 4, 12),
      (NEWLINE, 4, 12, 5, 1),
      (COMMENTLITKIND, 5, 1, 7, 11),
      (NEWLINE, 7, 11, 8, 1),
      (EOF, 8, 1, 8, 1)
    )


    import Colors._
    val markings = Seq(
      Marking(Position(1, 5, 2, 5), Red),
      Marking(Position(5, 4, 5, 7), Bold + Yellow),
      Marking(Position(7, 7, 7, 11), Bold + Underlined + GreenBG)
    )

    val syntaxHighlighter = makeSyntaxHighlighter(lexer)
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

  private def makeSyntaxHighlighter(lexer: Lexer, colorScheme: ColorScheme = DefaultColorScheme): SyntaxHighlighter = {
    val formatting = Formatting(colorScheme = colorScheme)
    SyntaxHighlighter(lexer, formatting)
  }

  private def tokenList(values: (TokenKind, Int, Int, Int, Int)*): List[Token] = {
    values.map { case (tokenKind, line, col, endLine, endCol) =>
      token(tokenKind, line, col, endLine, endCol)
    }.toList
  }

  private def token(tokenKind: TokenKind, line: Int, col: Int, endLine: Int, endCol: Int) = {
    val source = mock[Source]
    val token = tokenKind match {
      case IDKIND         => new ID("")
      case INTLITKIND     => new INTLIT(0)
      case LONGLITKIND    => new LONGLIT(0)
      case FLOATLITKIND   => new FLOATLIT(0)
      case DOUBLELITKIND  => new DOUBLELIT(0)
      case CHARLITKIND    => new CHARLIT('a')
      case STRLITKIND     => new STRLIT("")
      case COMMENTLITKIND => new COMMENTLIT("")
      case _              => new Token(tokenKind)
    }
    token.setPos(source, line, col, endLine, endCol)
  }

  // This function was used to generate the tokens in the above tests
  private def printTokens(code: String) = {
    val lexer = Lexer(VoidReporter(), ErrorStringContext())
    val tokens = lexer(StringSource(code.stripAnsi, ""))
    tokens
      .map(t => (t.kind.getClass.getSimpleName.dropRight(1), t.line, t.col, t.endLine, t.endCol))
      .mkString("," + System.lineSeparator)
      .print

  }
}
