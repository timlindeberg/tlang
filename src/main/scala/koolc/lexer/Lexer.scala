package koolc
package lexer

import utils._
import scala.io.Source
import java.io.File

object Lexer extends Pipeline[File, Iterator[Token]] {
  import Tokens._

  def run(ctx: Context)(f: File): Iterator[Token] = {
    val source = Source.fromFile(f)
    import ctx.reporter._

    // Complete this file

    new Iterator[Token] {
      def hasNext = {
        ???
      }

      def next = {
        ???
      }
    }

  }
}
