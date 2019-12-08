package tlang
package compiler
package output
package debug

import tlang.compiler.lexer.Token
import tlang.formatting.Formatter
import tlang.formatting.grid.Alignment.Center
import tlang.formatting.grid.{Column, TruncatedColumn}
import tlang.utils.JSON.Json

case class TokenOutput(phaseName: String, allTokens: List[List[Token]])(implicit formatter: Formatter) extends Output {
  override def pretty: String = {

    import formatter._

    val grid = formatter.grid.header(Bold("Output after ") + Blue(phaseName.capitalize) + Bold(" phase"))
    allTokens.foreach { tokens =>
      grid
        .row(alignment = Center)
        .content(tokens.head.sourceDescription)
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

  override def json: Json = {
    Json(
      "tokens" -> allTokens.map { tokens =>
        Json(
          "file" -> tokens.head.simpleSourceDescription,
          "tokens" -> tokens.map { token =>
            Json(
              "name" -> name(token),
              "start" -> Json(
                "line" -> token.line,
                "col" -> token.col
              ),
              "end" -> Json(
                "line" -> token.lineEnd,
                "col" -> token.colEnd
              )
            )
          }
        )
      }
    )
  }

  private def name(token: Token) = token.kind.getClass.getSimpleName.dropRight(1).replaceAll("KIND", "")
}
