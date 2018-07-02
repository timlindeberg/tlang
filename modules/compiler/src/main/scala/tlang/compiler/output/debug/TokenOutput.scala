package tlang.compiler.output.debug

import tlang.compiler.lexer.Token
import tlang.compiler.output.Output
import tlang.formatting.{Formatter, SimpleFormatting}
import tlang.formatting.grid.Alignment.Center
import tlang.formatting.grid.{Column, TruncatedColumn}
import tlang.utils.Extensions.NL

case class TokenOutput(phaseName: String, allTokens: List[List[Token]]) extends Output {
  override def pretty(formatter: Formatter): String = {
    val formatting = formatter.formatting
    import formatting._

    val grid = formatter.grid.header(Bold("Output after ") + Blue(phaseName.capitalize))
    allTokens.foreach { tokens =>
      grid
        .row(alignment = Center)
        .content(tokens.head.sourceDescription(formatting))
        .row(TruncatedColumn, Column, Column, Column)
        .columnHeaders("Text", "Token", "Start", "End")
        .mapContent(tokens) { token =>
          val tokenName = name(token)
          val start = NumColor(token.line) + ":" + NumColor(token.col)
          val end = NumColor(token.lineEnd) + ":" + NumColor(token.colEnd)
          (token.toString.replaceAll(NL, ""), Bold(tokenName), start, end)
        }
    }
    grid.render()
  }

  override def json(): Map[String, Any] = Map(
    "tokens" -> allTokens.map { tokens =>
      Map(
        "file" -> tokens.head.sourceDescription(SimpleFormatting),
        "tokens" -> tokens.map { token =>
          Map(
            "name" -> name(token),
            "start" -> Map(
              "line" -> token.line,
              "col" -> token.col
            ),
            "end" -> Map(
              "line" -> token.lineEnd,
              "col" -> token.colEnd
            )
          )
        }
      )
    }
  )

  private def name(token: Token) = token.kind.getClass.getSimpleName.dropRight(1).replaceAll("KIND", "")
}
