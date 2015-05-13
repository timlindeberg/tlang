package koolc
package lexer

import utils._
import scala.io.Source
import java.io.File

object PrintTokens extends Pipeline[Iterator[Token], Iterator[Token]] {
  import Tokens._

  def run(ctx: Context)(tokens: Iterator[Token]): Iterator[Token] = {
    val ts = for (t <- tokens) yield {
      println(t + "(" + t.line + ":" + t.col + ")")
      t
    }

    ts
  }
}
