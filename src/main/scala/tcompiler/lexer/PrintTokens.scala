package tcompiler
package lexer

import utils._
import scala.io.Source
import java.io.File

object PrintTokens extends Pipeline[List[Token], List[Token]] {
  import Tokens._

  def run(ctx: Context)(tokens: List[Token]): List[Token] = {
    val ts = for (t <- tokens) yield {
      println(t + "(" + t.line + ":" + t.col + ")")
      t
    }

    ts
  }
}
