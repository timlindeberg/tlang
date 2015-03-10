package koolc.lexer

import org.scalatest._
import java.io.File

import koolc.utils._
import scala.collection.mutable.ArrayBuffer

class LexerSpec extends FlatSpec with Matchers {

  it should "lex kool programs" in {
    val it = useLexer("valid-program-1.kool")
    
    var tokens = new ArrayBuffer[Token]()
    while (it.hasNext) tokens += it.next
    println("Got tokens")
    for(t <- tokens) {
      print(t+"("+t.line+":"+t.col+") ")
    }
    
  }

  def useLexer(fileName: String): Iterator[Token] = {
    val ctx = new Context(reporter = new koolc.utils.Reporter, file = new File("./src/test/resources/" + fileName), outDir = None)
    Lexer.run(ctx)(ctx.file)
  }
}